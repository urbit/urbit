import { Anchor, Text } from '@tlon/indigo-react';
import { Contact, Group } from '@urbit/api';
import React from 'react';
import ReactMarkdown, { ReactMarkdownProps } from 'react-markdown';
import RemarkDisableTokenizers from 'remark-disable-tokenizers';
import { isValidPatp } from 'urbit-ob';
import GlobalApi from '~/logic/api/global';
import { deSig } from '~/logic/lib/util';
import { PermalinkEmbed } from '~/views/apps/permalinks/embed';
import { Mention } from '~/views/components/MentionText';
import RemoteContent from '~/views/components/RemoteContent';

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

type RichTextProps = ReactMarkdownProps & {
  api?: GlobalApi;
  disableRemoteContent?: boolean;
  contact?: Contact;
  group?: Group;
  pending?: boolean;
  transcluded?: number;
  inline?: boolean;
  color?: string;
  children?: any;
  width?: string;
  display?: string[] | string;
  mono?: boolean;
  mb?: number;
  minWidth?: number | string;
  maxWidth?: number | string;
  flexShrink?: number;
  textOverflow?: string;
  overflow?: string;
  whiteSpace?: string;
  gray?: boolean;
  title?: string;
  py?: number;
  overflowX?: any;
  verticalAlign?: any;
};

const RichText = React.memo(({ disableRemoteContent = false, api, ...props }: RichTextProps) => (
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
        if (!disableRemoteContent) {
          // @ts-ignore RemoteContent weirdness
          return <RemoteContent className="mw-100" url={linkProps.href} />;
        }

        return (
          <Anchor
            display="inline"
            target='_blank'
            rel='noreferrer noopener'
            borderBottom='1px solid'
            remoteContentPolicy={remoteContentPolicy}
            onClick={(e) => {
              e.stopPropagation();
            }}
            {...linkProps}
          >{linkProps.children}</Anchor>
        );
      },
      linkReference: (linkProps): any => {
        const linkText = String(linkProps.children[0].props.children);
        if (isValidPatp(linkText)) {
          return <Mention ship={deSig(linkText)} api={api} />;
        } else if(linkText.startsWith('web+urbitgraph://')) {
          return (
            <PermalinkEmbed
              pending={props.pending}
              link={linkText}
              transcluded={props.transcluded}
              api={api}
            />
          );
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
            paddingLeft={2} {...props}
          >
            {blockquoteProps.children}
          </Text>
        );
      },
      paragraph: (paraProps) => {
        return <Text display={props.inline ? 'inline' : 'block'} mb={2} {...props}>{paraProps.children}</Text>;
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
