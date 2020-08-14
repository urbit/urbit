import React, { Component } from 'react';

import TextContent from './content/text';
import CodeContent from './content/code';
import UrlContent from './content/url';


export default class MessageContent extends Component {

  render() {
    const { props } = this;

    const content = props.letter;

    if ('code' in content) {
      return <CodeContent content={content} />;
    } else if ('url' in content) {
      return <UrlContent content={content} />;
    } else if ('me' in content) {
      return (
        <p className='f7 i lh-copy v-top'>
          {content.me}
        </p>
      );
    }
    else if ('text' in content) {
      return <TextContent content={content} />;
    } else {
      return null;
    }
  }

}
