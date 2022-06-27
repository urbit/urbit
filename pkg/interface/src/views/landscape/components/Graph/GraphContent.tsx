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

const addEmphasisToMention = (
  contents: Content[],
  content: Content,
  index: number
) => {
  const prevContent = contents[index - 1];
  const nextContent = contents[index + 1];

  if (
    'text' in content &&
    (content.text.trim() === '**' || content.text.trim() === '*')
  ) {
    return {
      text: ''
    };
  }
  if (
    'text' in content &&
    content.text.endsWith('*') &&
    !content.text.startsWith('*') &&
    nextContent !== undefined &&
    'mention' in nextContent
  ) {
    if (content.text.charAt(content.text.length - 2) === '*') {
      return { text: content.text.slice(0, content.text.length - 2) };
    }
    return { text: content.text.slice(0, content.text.length - 1) };
  }
  if (
    'text' in content &&
    content.text.startsWith('*') &&
    !content.text.endsWith('*') &&
    prevContent !== undefined &&
    'mention' in contents[index - 1]
  ) {
    if (content.text.charAt(1) === '*') {
      return { text: content.text.slice(2, content.text.length) };
    }
    return { text: content.text.slice(1, content.text.length) };
  }
  if (
    'mention' in content &&
    prevContent !== undefined &&
    'text' in prevContent &&
    // @ts-ignore type guard above covers this.
    prevContent.text.endsWith('*') &&
    nextContent !== undefined &&
    'text' in contents[index + 1] &&
    // @ts-ignore type guard above covers this.
    nextContent.text.startsWith('*')
  ) {
    if (
      // @ts-ignore covered by typeguard in conditions
      prevContent.text.charAt(prevContent.text.length - 2) === '*' &&
      // @ts-ignore covered by typeguard in conditions
      nextContent.text.charAt(nextContent.text[1]) === '*'
    ) {
      return {
        mention: content.mention,
        emphasis: 'bold'
      };
    }
    return {
      mention: content.mention,
      emphasis: 'italic'
    };
  }
  return content;
};

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

const contentToMdAst =
  (tall: boolean) =>
  (content: Content): [StitchMode, any] => {
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
      const images = ['.jpg', '.jpeg', '.png', '.gif', '.webp'];
      return [
        'inline',
        {
          type: 'root',
          children: [
            {
              type: 'link',
              url: content.url,
              children: [
                {
                  type: 'text',
                  value: !images.some(i => content.url.includes(i))
                    ? content.url
                    : ''
                }
              ]
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
              ship: content.mention,
              emphasis: content.emphasis
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

  // wrap bare link in list-item inside a p node
  // for better typography consistency
  if (last?.type === 'listItem') {
    if (last?.children.length === 0) {
      last.children.push({
        type: 'paragraph',
        children: []
      });
    }
  }
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
  return [
    'paragraph',
    'heading',
    'list',
    'listItem',
    'table',
    'blockquote'
  ].includes(node.type)
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
  const t = _.reduce(
    asts,
    ([prevMode, ast], [mode, val]): [StitchMode, GraphAstNode] => {
      if (prevMode === 'block' || prevMode === 'inline') {
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

  t[1].children.map((c, idx) => {
    const links = [];
    function addRichEmbedURL(nodes) {
      if (nodes?.children) {
        nodes.children.filter(k => {
          if (k.type === 'link') {
            links.push({
              type: 'root',
              children: [
                {
                  type: 'graph-url',
                  url: k.url
                }
              ]
            });
          } else if (k?.children) {
            k.children.filter(o => {
              if (o.type === 'link') {
                links.push({
                  type: 'root',
                  children: [
                    {
                      type: 'graph-url',
                      url: o.url
                    }
                  ]
                });
              }
            });
          }
        });

        nodes.children.push(...links);
      }
    }
    addRichEmbedURL(c);
  });

  return t;
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
  paragraph: ({ children }) => {
    return (
      <Box display="block">
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
  link: props => {
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
    return ordered ? (
      <Ol fontSize="1">{children}</Ol>
    ) : (
      <Ul fontSize="1">{children}</Ul>
    );
  },
  'graph-mention': obj => {
    return <Mention ship={obj.ship} emphasis={obj.emphasis} />;
  },
  image: ({ url, tall }) => (
    <Box mt="1" mb="2" flexShrink={0}>
      <RemoteContent key={url} url={url} tall={tall} />
    </Box>
  ),
  'graph-url': ({ url, tall }) => (
    <RemoteContent key={url} url={url} tall={tall} />
  ),
  'graph-reference': ({ reference, transcluded }) => {
    const { link } = referenceToPermalink({ reference });
    return (
      <Box my={2} flexShrink={0}>
        <PermalinkEmbed link={link} transcluded={transcluded} showOurContact />
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
      tall={tall}
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

export const GraphContent = React.memo((props: GraphContentProps) => {
  const { contents, tall = false, transcluded = 0, ...rest } = props;
  const [, ast] = stitchAsts(
    contents
      .map((content, index) => addEmphasisToMention(contents, content, index))
      .map(contentToMdAst(tall))
  );
  return (
    <Box {...rest}>
      <Graphdown transcluded={transcluded} ast={ast} tall={tall} />
    </Box>
  );
});
