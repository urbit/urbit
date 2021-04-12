import React, { Component, useState, useEffect } from 'react';
import { Link } from 'react-router-dom';
import ReactMarkdown from 'react-markdown';
import RemarkDisableTokenizers from 'remark-disable-tokenizers';
import urbitOb from 'urbit-ob';
import { Text } from '@tlon/indigo-react';
import { GroupLink } from '~/views/components/GroupLink';
import { Row } from '@tlon/indigo-react';

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

const DISABLED_INLINE_TOKENS = [
  'autoLink',
  'url',
  'email',
  'link',
  'reference'
];

const renderers = {
  inlineCode: ({ language, value }) => {
    return (
      <Text
        mono
        p='1'
        backgroundColor='washedGray'
        fontSize='0'
        style={{ whiteSpace: 'preWrap' }}
      >
        {value}
      </Text>
    );
  },
  blockquote: ({ children }) => {
    return (
      <Text
        lineHeight="20px"
        display="block"
        borderLeft="1px solid"
        color="black"
        paddingLeft={2}>
        {children}
      </Text>
    )
  },
  paragraph: ({ children }) => {
    return (
      <Text fontSize='1' lineHeight={'20px'}>
        {children}
      </Text>
    );
  },
  code: ({ language, value }) => {
    return (
      <Text
        p='1'
        className='clamp-message'
        display='block'
        borderRadius='1'
        mono
        fontSize='0'
        backgroundColor='washedGray'
        overflowX='auto'
        style={{ whiteSpace: 'pre' }}
      >
        {value}
      </Text>
    );
  }
};

const MessageMarkdown = React.memo((props) => {
  const { source, ...rest } = props;
  const blockCode = source.split('```');
  const codeLines = blockCode.map((codes) => codes.split('\n'));
  const lines = codeLines.reduce((acc, val, i) => {
    if (i % 2 === 1) {
      return [...acc, `\`\`\`${val.join('\n')}\`\`\``];
    } else {
      return [...acc, ...val];
    }
  }, []);

  return lines.map((line, i) => (
    <React.Fragment key={i}>
      {i !== 0 && <Row height={2} />}
      <ReactMarkdown
        {...rest}
        source={line}
        unwrapDisallowed={true}
        renderers={renderers}
        allowNode={(node, index, parent) => {
          if (
            node.type === 'blockquote' &&
            parent.type === 'root' &&
            node.children.length &&
            node.children[0].type === 'paragraph' &&
            node.children[0].position.start.offset < 2
          ) {
            node.children[0].children[0].value =
              '>' + node.children[0].children[0].value;
            return false;
          }

          return true;
        }}
        plugins={[
          [
            RemarkDisableTokenizers,
            {
              block: DISABLED_BLOCK_TOKENS,
              inline: DISABLED_INLINE_TOKENS
            }
          ]
        ]}
      />
    </React.Fragment>
  ));
});

export default function TextContent(props) {
  const content = props.content;

  const group = content.text.match(
    /([~][/])?(~[a-z]{3,6})(-[a-z]{6})?([/])(([a-z0-9-])+([/-])?)+/
  );
  const isGroupLink =
    group !== null && // matched possible chatroom
    group[2].length > 2 && // possible ship?
    urbitOb.isValidPatp(group[2]) && // valid patp?
    group[0] === content.text; // entire message is room name?

  if (isGroupLink) {
    const resource = `/ship/${content.text}`;
    return (
      <GroupLink
        resource={resource}
        api={props.api}
        pl='2'
        border='1'
        borderRadius='2'
        borderColor='washedGray'
      />
    );
  } else {
    return (
      <Text
        flexShrink={0}
        color='black'
        fontSize={props.fontSize ? props.fontSize : '14px'}
        lineHeight={props.lineHeight ? props.lineHeight : '20px'}
        style={{ overflowWrap: 'break-word' }}
      >
        <MessageMarkdown source={content.text} />
      </Text>
    );
  }
}
