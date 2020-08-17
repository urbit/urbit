import React, { Component } from 'react';
import { Link } from 'react-router-dom';
import ReactMarkdown from 'react-markdown';
import RemarkDisableTokenizers from 'remark-disable-tokenizers';
import urbitOb from 'urbit-ob';

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

const MessageMarkdown = React.memo(props => (
  <ReactMarkdown
    {...props}
    plugins={[[
      RemarkDisableTokenizers,
      { block: DISABLED_BLOCK_TOKENS, inline: DISABLED_INLINE_TOKENS }
    ]]} />
));


export default class TextContent extends Component {

  render() {
    const { props } = this;
    const content = props.content;

    const group = content.text.match(
      /([~][/])?(~[a-z]{3,6})(-[a-z]{6})?([/])(([a-z0-9-])+([/-])?)+/
    );
    if ((group !== null) // matched possible chatroom
      && (group[2].length > 2) // possible ship?
      && (urbitOb.isValidPatp(group[2]) // valid patp?
      && (group[0] === content.text))) { // entire message is room name?
      return (
        <Link
          className="bb b--black b--white-d f7 mono lh-copy v-top"
          to={'/~groups/join/' + group.input}>
          {content.text}
        </Link>
      );
    } else {
      return (
        <section className="chat-md-message">
          <MessageMarkdown source={content.text} />
        </section>
      );
    }
  }
}
