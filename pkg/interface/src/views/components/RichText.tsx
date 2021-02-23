import React from 'react';
import { hasProvider } from 'oembed-parser';
import ReactMarkdown from 'react-markdown';
import { isValidPatp } from 'urbit-ob';
import RemarkDisableTokenizers from 'remark-disable-tokenizers';

import { BaseAnchor, Text } from '@tlon/indigo-react';
import { deSig } from '@urbit/api';

import RemoteContent from '~/views/components/RemoteContent';
import { Mention } from '~/views/components/MentionText';
import useContactState from '~/logic/state/contacts';
import { Contact } from '@urbit/api';
import { Group } from '@urbit/api';

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

interface RichTextProps {
  disableRemoteContent: boolean;
  contact: Contact;
  group: Group;
  inline: boolean;
  color: string;
}

const RichText = React.memo(({ disableRemoteContent, ...props }: RichTextProps) => {
  const contacts = useContactState(state => state.contacts);
  return (
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

          return <BaseAnchor target='_blank' rel='noreferrer noopener' borderBottom='1px solid' remoteContentPolicy={remoteContentPolicy} {...linkProps}>{linkProps.children}</BaseAnchor>;
        },
        linkReference: (linkProps) => {
          const linkText = String(linkProps.children[0].props.children);
          if (isValidPatp(linkText)) {
            return <Mention contacts={contacts || {}} contact={props.contact || {}} group={props.group} ship={deSig(linkText)} />;
          }
          return linkText;
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
  );
});

RichText.displayName = 'RichText';

export default RichText;
