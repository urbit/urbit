import React, { Component } from 'react';
import ReactMarkdown from 'react-markdown';
import RemarkDisableTokenizers from 'remark-disable-tokenizers';

const DISABLED_BLOCK_TOKENS = [
  'indentedCode',
  'blockquote',
  'atxHeading',
  'thematicBreak',
  'list',
  'setextHeading',
  'html',
  'definition',
  'table'
];

const DISABLED_INLINE_TOKENS = [
  'autoLink',
  'url',
  'email',
  'link',
  'reference'
];

const MessageMarkdown = React.memo(
  props => (<ReactMarkdown
              {...props}
              plugins={[[RemarkDisableTokenizers, { block: DISABLED_BLOCK_TOKENS, inline: DISABLED_INLINE_TOKENS }]]}
            />));


export default class TextContent extends Component {

  render() {
    const { props } = this;
    const font = !!props.isParent ? "f6" : "f7";

    const content = props.content;
    return (
      <section>
        <MessageMarkdown
          source={content.text}
        />
      </section>
    );
  }
}
