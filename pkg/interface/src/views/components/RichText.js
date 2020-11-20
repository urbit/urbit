import React from 'react';
import RemoteContent from '~/views/components/RemoteContent';
import { hasProvider } from 'oembed-parser';
import ReactMarkdown from 'react-markdown';
import RemarkDisableTokenizers from 'remark-disable-tokenizers';

import { BaseAnchor, Text } from '@tlon/indigo-react';

const DISABLED_BLOCK_TOKENS = [
  'indentedCode',
  'atxHeading',
  'thematicBreak',
  'list',
  'setextHeading',
  'html',
  'definition',
  'table'
];

const DISABLED_INLINE_TOKENS = [];

const RichText = React.memo(({ remoteContentPolicy, ...props }) => (
  <ReactMarkdown
    {...props}
    renderers={{
      link: (props) => {
        if (hasProvider(props.href)) {
          return <RemoteContent className="mw-100" url={props.href} remoteContentPolicy={remoteContentPolicy} />;
        }
        return <BaseAnchor target='_blank' rel='noreferrer noopener' borderBottom='1px solid' {...props}>{props.children}</BaseAnchor>;
      },
      paragraph: (paraProps) => {
        return <Text display={props.inline ? 'inline' : 'block'} mb='2' {...props}>{paraProps.children}</Text>;
      }
    }}
    plugins={[[
      RemarkDisableTokenizers,
      { block: DISABLED_BLOCK_TOKENS, inline: DISABLED_INLINE_TOKENS }
    ]]}
  />
));

RichText.displayName = 'RichText';

export default RichText;
