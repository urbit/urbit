import React from 'react';
import RemoteContent from '~/views/components/RemoteContent';
import { hasProvider } from 'oembed-parser';
import ReactMarkdown from 'react-markdown';
import RemarkDisableTokenizers from 'remark-disable-tokenizers';
import { Anchor, Text } from '@tlon/indigo-react';
import { isValidPatp } from 'urbit-ob';

import { deSig } from '~/logic/lib/util';
import { Mention } from '~/views/components/MentionText';

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

const RichText = React.memo(({ disableRemoteContent, ...props }) => (
  <ReactMarkdown
    {...props}
    renderers={{
      link: (linkProps) => {
        const remoteContentPolicy = disableRemoteContent ? {
          imageShown: false,
          audioShown: false,
          videoShown: false,
          oembedShown: false
        } : null;
        if (!disableRemoteContent && hasProvider(linkProps.href)) {
          return <RemoteContent className="mw-100" url={linkProps.href} />;
        }

        return <Anchor display="inline" target='_blank' rel='noreferrer noopener' borderBottom='1px solid' remoteContentPolicy={remoteContentPolicy} {...linkProps}>{linkProps.children}</Anchor>;
      },
      linkReference: (linkProps) => {
        const linkText = String(linkProps.children[0].props.children);
        if (isValidPatp(linkText)) {
          return <Mention contact={props.contact || {}} group={props.group} ship={deSig(linkText)} />;
        }
        return linkText;
      },
      blockquote: (blockquoteProps) => {
        return (
          <Text
            lineHeight="20px"
            display="block"
            borderLeft="1px solid"
            color="black"
            paddingLeft={2} {...props}>
            {blockquoteProps.children}
          </Text>
        )
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
