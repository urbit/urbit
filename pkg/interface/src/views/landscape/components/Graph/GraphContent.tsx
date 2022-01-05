import {
  Anchor,
  Box,
  H1,
  H2,
  H3,
  H4,
  Text,
  Li,
  Ol,
  Ul,
  Table,
  Tr,
  Td
} from '@tlon/indigo-react';
import { Content, CodeContent } from '@urbit/api';
import _ from 'lodash';
import { BlockContent, Content as AstContent, Parent, Root } from 'ts-mdast';
import React from 'react';
import { referenceToPermalink } from '~/logic/lib/permalinks';
import { PropFunc } from '~/types';
import { PermalinkEmbed } from '~/views/apps/permalinks/embed';
import { Mention } from '~/views/components/MentionText';
import RemoteContent from '~/views/components/RemoteContent';
import { parseTall, parseWide } from './parse';

type StitchMode = 'merge' | 'block' | 'inline';

// XX make better
type GraphAstNode = any;

interface GraphMentionNode {
  type: 'graph-mention';
  ship: string;
}

const codeToMdAst = (content: CodeContent) => {
  return {
    type: 'root',
    children: [
      {
        type: 'code',
        value: content.code.expression
      },
      {
        type: 'code',
        value: (content.code.output || []).join('\n')
      }
    ]
  };
};

const contentToMdAst = (tall: boolean) => (
  content: Content
): [StitchMode, any] => {
  if ('text' in content) {
    if (content.text.toString().trim().length === 0) {
      return [
        'merge',
        { type: 'root', children: [{ type: 'paragraph', children: [] }] }
      ];
    }
    return [
      'merge',
      tall ? parseTall(content.text) : parseWide(content.text)
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
            reference: content.reference
          }
        ]
      }
    ];
  } else if ('url' in content) {
    return [
      'block',
      {
        type: 'root',
        children: [
          {
            type: 'graph-url',
            url: content.url
          }
        ]
      }
    ];
  } else if ('mention' in content) {
    return [
      'inline',
      {
        type: 'root',
        children: [
          {
            type: 'graph-mention',
            ship: content.mention
          }
        ]
      }
    ];
  }
  return [
    'inline',
    {
      type: 'root',
      children: []
    }
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
        ...a.children.slice(lastParaIdx + 1)
      ]
    };
    return ros;
  }
  const res = { ...a, children: [...a.children, ...b] };
  return res;
}

function last<T>(arr: T[]) {
  return arr[arr.length - 1];
}

function getChildren<T extends unknown>(node: T): AstContent[] {
  // @ts-ignore TODO @liam-fitzgerald
  if ('children' in node) {
    // @ts-ignore TODO @liam-fitzgerald
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
      children: [...aGrandchild, ...bGrandchild]
    };
    return {
      ...a,
      children: [...aChildren.slice(0, -1), mergedPara, ...bChildren.slice(1)]
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
    children: [...a.children, { type: 'paragraph', children: b }]
  };
}

function stitchAsts(asts: [StitchMode, GraphAstNode][]) {
  return _.reduce(
    asts,
    ([prevMode, ast], [mode, val]): [StitchMode, GraphAstNode] => {
      if (prevMode === 'block') {
        if (mode === 'inline') {
          return [mode, stitchInlineAfterBlock(ast, val?.children ?? [])];
        }
        if (mode === 'merge') {
          return [mode, stitchMerge(ast, val)];
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
    ['block', { type: 'root', children: [] }] as [StitchMode, GraphAstNode]
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
    return <br />;
  },
  thematicBreak: () => {
    return <Box display="block" width="100%" height={2}></Box>;
  },
  inlineCode: ({ language, value }) => {
    return (
      <Text
        mono
        fontWeight="inherit"
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
    return <b>{children}</b>;
  },
  emphasis: ({ children }) => {
    return <i>{children}</i>;
  },
  delete: ({ children }) => {
    return <del> {children}</del>;
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
        my={1}
      >
        {children}
      </Text>
    );
  },
  paragraph: ({ children, collapsed = false }) => {
    const containerStyle = collapsed
      ? { whiteSpace: 'nowrap', overflow: 'hidden', textOverflow: 'ellipsis' }
      : {};
    return (
      <Box display="block" {...containerStyle}>
        <Text
          fontSize={1}
          lineHeight="tall"
          style={{ overflowWrap: 'break-word' }}
        >
          {children}
        </Text>
      </Box>
    );
  },
  table: ({ children }) => <Table>{children}</Table>,
  tableRow: ({ children }) => <Tr>{children}</Tr>,
  tableCell: ({ children }) => (
    <Td>
      <Text fontSize="1" lineHeight="tall">
        {children}
      </Text>
    </Td>
  ),
  listItem: ({ children }) => {
    return <Li>{children}</Li>;
  },
  code: ({ language, tall, value, ...rest }) => {
    const inner = (
      <Text
        p={1}
        className="clamp-message"
        display="block"
        borderRadius={1}
        fontWeight="inherit"
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
        fontSize="inherit"
        fontWeight="inherit"
        color="black"
        target="_blank"
      >
        {props.children}
      </Anchor>
    );
  },
  list: ({ depth, ordered, children }) => {
    return ordered ? <Ol>{children}</Ol> : <Ul>{children}</Ul>;
  },
  'graph-mention': ({ ship }) => <Mention ship={ship} />,
  image: ({ url, tall }) => (
    <Box mt="1" mb="2" flexShrink={0}>
      <RemoteContent key={url} url={url} tall={tall} />
    </Box>
  ),
  'graph-url': ({ url, tall }) => (
    <Box mt={1} mb={2} flexShrink={0}>
      <RemoteContent key={url} url={url} tall={tall} />
    </Box>
  ),
  'graph-reference': ({ reference, transcluded }) => {
    const { link } = referenceToPermalink({ reference });
    if (transcluded > 1) {
      return null;
    }
    return (
      <Box my={2} flexShrink={0}>
        <PermalinkEmbed
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
  )
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
    >
      {children.map((c, idx) => (
        <Graphdown
          key={idx}
          transcluded={transcluded}
          depth={depth + 1}
          tall={tall}
          {...rest}
          ast={c}
        />
      ))}
    </Renderer>
  );
}

export type GraphContentProps = PropFunc<typeof Box> & {
  tall?: boolean;
  transcluded?: number;
  contents: Content[];
  showOurContact: boolean;
};

export const GraphContent = React.memo((
  props: GraphContentProps
) => {
  const {
    contents,
    tall = false,
    transcluded = 0,
    collapsed = false,
    ...rest
  } = props;
  const [, ast] = stitchAsts(contents.map(contentToMdAst(tall)));
  return (
    <Box {...rest}>
      <Graphdown transcluded={transcluded} ast={ast} tall={tall} collapsed={collapsed} />
    </Box>
  );
});
