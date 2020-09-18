import React from "react";
import RemoteContent from "~/views/components/RemoteContent";
import { hasProvider } from 'oembed-parser';
import ReactMarkdown from 'react-markdown';
import RemarkDisableTokenizers from 'remark-disable-tokenizers';

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

const RichText = React.memo(({remoteContentPolicy, ...props}) => (
  <ReactMarkdown
    {...props}
    renderers={{
      link: (props) => {
        if (hasProvider(props.href)) {
          return <RemoteContent className="mw-100" url={props.href} remoteContentPolicy={remoteContentPolicy}/>;
        }
        return <a {...props} className="bb b--white-d b--black">{props.children}</a>
      },
      paragraph: (props) => {
        return <p {...props} className="mb2 lh-copy">{props.children}</p>
      }
    }}
    plugins={[[
      RemarkDisableTokenizers,
      { block: DISABLED_BLOCK_TOKENS, inline: DISABLED_INLINE_TOKENS }
    ]]} />
));

export default RichText;