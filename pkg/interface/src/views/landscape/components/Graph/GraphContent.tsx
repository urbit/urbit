import {
  Anchor, Box,
  Col, H1,
  H2,
  H3,
  H4, Text
} from '@tlon/indigo-react';
import { Content, ReferenceContent } from '@urbit/api';
import _ from 'lodash';
import {
  BlockContent, Content as AstContent, Parent, Root
} from 'ts-mdast';
import React from 'react';
import GlobalApi from '~/logic/api/global';
import { referenceToPermalink } from '~/logic/lib/permalinks';
import { PropFunc } from '~/types';
import { PermalinkEmbed } from '~/views/apps/permalinks/embed';
import Dot from '~/views/components/Dot';
import { Mention } from '~/views/components/MentionText';
import RemoteContent from '~/views/components/RemoteContent';
import CodeContent from './content/code';
import { parseTall, parseWide } from './parse';


type StitchMode = 'merge' | 'block' | 'inline';

// XX make better
type GraphAstNode = any;

interface GraphMentionNode {
  type: 'graph-mention';
  ship: string;
}
interface GraphRefereceNode {
  type: 'graph-reference';
  reference: ReferenceContent;
}

interface GraphUrl {
  type: 'graph-url';
  url: string;
}
const codeToMdAst = (content: CodeContent) => {
  return {
    type: 'root',
    children: [
      {
        type: 'code',
        value: content.code.expression,
      },
      {
        type: 'code',
        value: (content.code.output || []).join('\n'),
      },
    ],
  };
};

const contentToMdAst = (tall: boolean) => (
  content: Content
): [StitchMode, any] => {
  if ('text' in content) {
    return [
      'merge',
      tall ? parseTall(content.text) : parseWide(content.text),
    ] as [StitchMode, any];
  } else if ('code' in content) {
    return ['block', codeToMdAst(content)];
  } else if ('reference' in content) {
    return [
      'block',
      {
        type: 'root',
        children: [
          {
            type: 'graph-reference',
            reference: content.reference,
          },
        ],
      },
    ];
  } else if ('url' in content) {
    return [
      'block',
      {
        type: 'root',
        children: [
          {
            type: 'graph-url',
            url: content.url,
          },
        ],
      },
    ];
  } else if ('mention' in content) {
    return [
      'inline',
      {
        type: 'root',
        children: [
          {
            type: 'graph-mention',
            ship: content.mention,
          },
        ],
      },
    ];
  }
  return [
    'inline',
    {
      type: 'root',
      children: [],
    },
  ];
};

function stitchInline(a: any, b: any) {
  if (!a?.children) {
    throw new Error('Bad stitchInline call: missing root');
  }
  const lastParaIdx = a.children.length - 1;
  const last = a.children[lastParaIdx];
  if (last?.children) {
    const ros = {
      ...a,
      children: [
        ...a.children.slice(0, lastParaIdx),
        stitchInline(last, b),
        ...a.children.slice(lastParaIdx + 1),
      ],
    };
    return ros;
  }
  const res = { ...a, children: [...a.children, ...b] };
  return res;
}

function last<T>(arr: T[]) {
  return arr[arr.length - 1];
}

function getChildren<T extends {}>(node: T): AstContent[] {
  if ('children' in node) {
    // @ts-ignore
    return node.children;
  }
  return [];
}

export function asParent<T extends BlockContent>(node: T): Parent | undefined {
  return ['paragraph', 'heading', 'list', 'listItem', 'table'].includes(
    node.type
  )
    ? (node as Parent)
    : undefined;
}

function stitchMerge(a: Root, b: Root) {
  const aChildren = a.children;
  const bChildren = b.children;
  const lastType = last(aChildren)?.type;

  if (lastType === bChildren[0]?.type) {
    const aGrandchild = getChildren(last(aChildren));
    const bGrandchild = getChildren(bChildren[0]);
    const mergedPara = {
      ...last(aChildren),
      children: [...aGrandchild, ...bGrandchild],
    };
    return {
      ...a,
      children: [...aChildren.slice(0, -1), mergedPara, ...bChildren.slice(1)],
    };
  }
  return { ...a, children: [...aChildren, ...bChildren] };
}

function stitchBlock(a: Root, b: AstContent[]) {
  return { ...a, children: [...a.children, ...b] };
}

function stitchInlineAfterBlock(a: Root, b: GraphMentionNode[]) {
  return {
    ...a,
    children: [...a.children, { type: 'paragraph', children: b }],
  };
}

function stitchAsts(asts: [StitchMode, GraphAstNode][]) {
  return _.reduce(
    asts.slice(1),
    ([prevMode, ast], [mode, val]): [StitchMode, GraphAstNode] => {
      if (prevMode === 'block') {
        if (mode === 'inline') {
          return [mode, stitchInlineAfterBlock(ast, val?.children ?? [])];
        }
        if (mode === 'merge') {
          return [mode, stitchBlock(ast, val?.children ?? [])];
        }
        if (mode === 'block') {
          return [mode, stitchBlock(ast, val?.children ?? [])];
        }
      }
      if (mode === 'inline') {
        return [mode, stitchInline(ast, val?.children ?? [])];
      }
      if (mode === 'merge') {
        return [mode, stitchMerge(ast, val)];
      }
      if (mode === 'block') {
        return [mode, stitchBlock(ast, val?.children ?? [])];
      }
      return [mode, ast];
    },
    asts[0]
  );
}
const header = ({ children, depth, ...rest }) => {
  const level = depth;
  const inner =
    level === 1 ? (
      <H1 display="block">{children}</H1>
    ) : level === 2 ? (
      <H2 display="block">{children}</H2>
    ) : level === 3 ? (
      <H3 display="block">{children}</H3>
    ) : (
      <H4 display="block">{children}</H4>
    );
  return <Box {...rest}>{inner}</Box>;
};

const renderers = {
  heading: header,
  break: () => {
    return <Box display="block" width="100%" height={2}></Box>;
  },
  thematicBreak: () => {
    return <Box display="block" width="100%" height={2}></Box>;
  },
  inlineCode: ({ language, value }) => {
    return (
      <Text
        mono
        p={1}
        backgroundColor="washedGray"
        fontSize={0}
        style={{ whiteSpace: 'pre-wrap' }}
      >
        {value}
      </Text>
    );
  },
  strong: ({ children }) => {
    return (
      <Text fontWeight="bold" lineHeight="1">
        {children}
      </Text>
    );
  },
  emphasis: ({ children }) => {
    return (
      <Text fontStyle="italic" fontSize={1} lineHeight="tall">
        {children}
      </Text>
    );
  },
  blockquote: ({ children, depth, tall, ...rest }) => {
    if (depth > 1) {
      return children;
    }

    return (
      <Text
        lineHeight="tall"
        display="block"
        borderLeft="1px solid"
        color="black"
        paddingLeft={2}
        py={1}
        mb={1}
      >
        {children}
      </Text>
    );
  },
  paragraph: ({ children }) => {
    return (
      <Text fontSize={1} lineHeight="tall">
        {children}
      </Text>
    );
  },
  listItem: ({ children }) => {
    return (
      <Box position="relative" alignItems="center">
        <Dot
          top="7px"
          position="absolute"
          left="0px"
          mr={1}
          height="20px"
          width="20px"
        />
        <Box ml={2}>{children}</Box>
      </Box>
    );
  },

  code: ({ language, tall, value, ...rest }) => {
    console.log(rest);
    const inner = (
      <Text
        p={1}
        className="clamp-message"
        display="block"
        borderRadius={1}
        mono
        fontSize={0}
        backgroundColor="washedGray"
        overflowX="auto"
        style={{ whiteSpace: 'pre' }}
      >
        {value}
      </Text>
    );
    return tall ? <Box mb={2}>{inner}</Box> : inner;
  },
  link: (props) => {
    return (
      <Anchor
        display="inline"
        href={props.url}
        borderBottom="1"
        color="black"
        target="_blank"
      >
        {props.children}
      </Anchor>
    );
  },
  list: ({ depth, children }) => {
    return (
      <Col ml={3} gapY={2} my={2}>
        {children}
      </Col>
    );
  },
  'graph-mention': ({ ship }) => <Mention api={{} as any} ship={ship} />,
  image: ({ url }) => (
    <Box mt="1" mb="2" flexShrink={0}>
      <RemoteContent
      // @ts-ignore RemoteContent weirdness
      key={url} url={url}
      />
    </Box>
  ),
  'graph-url': ({ url }) => (
    <Box mt={1} mb={2} flexShrink={0}>
      <RemoteContent
      // @ts-ignore RemoteContent weirdness
        key={url} url={url}
      />
    </Box>
  ),
  'graph-reference': ({ api, reference, transcluded }) => {
    const { link } = referenceToPermalink({ reference });
    return (
      <Box mb={2} flexShrink={0}>
        <PermalinkEmbed
          api={api}
          link={link}
          transcluded={transcluded}
          showOurContact
        />
      </Box>
    );
  },
  root: ({ tall, children }) =>
    tall ? (
      <Box
        display="grid"
        style={{ gridTemplateColumns: 'minmax(0,1fr)', rowGap: '1rem' }}
      >
        {children}
      </Box>
    ) : (
      <Box>{children}</Box>
    ),
  text: ({ value }) => (
    <>
      {value.split('\n').map((v, idx) => (
        <React.Fragment key={idx}>
          {idx !== 0 ? <br /> : null}
          {v}
        </React.Fragment>
      ))}
    </>
  ),
};

export function Graphdown<T extends {} = {}>(
  props: {
    ast: GraphAstNode;
    transcluded: number;
    tall?: boolean;
    depth?: number;
  } & T
) {
  const { ast, transcluded, tall, depth = 0, ...rest } = props;
  const { type, children = [], ...nodeRest } = ast;
  const Renderer = renderers[ast.type] ?? (() => `unknown element: ${type}`);

  return (
    <Renderer
      transcluded={transcluded}
      depth={depth}
      {...rest}
      {...nodeRest}
      tall={tall}
    >
      {children.map((c, idx) => (
        <Graphdown
          key={idx}
          transcluded={transcluded}
          depth={depth + 1}
          {...rest}
          ast={c}
        />
      ))}
    </Renderer>
  );
}

export const GraphContent = React.memo(function GraphContent(
  props: {
    tall?: boolean;
    transcluded?: number;
    contents: Content[];
    api: GlobalApi;
    showOurContact: boolean;
  } & PropFunc<typeof Box>
) {
  const {
    post,
    contents,
    tall = false,
    transcluded = 0,
    showOurContact,
    api,
    ...rest
  } = props;
  const [, ast] = stitchAsts(contents.map(contentToMdAst(tall)));
  return (
    <Box {...rest}>
      <Graphdown transcluded={transcluded} api={api} ast={ast} tall={tall} />
    </Box>
  );
});
