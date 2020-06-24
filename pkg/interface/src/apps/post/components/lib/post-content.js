import React, { Component } from 'react';

import TextContent from './content/text';
import CodeContent from './content/code';
import UrlContent from './content/url';


export default class PostContent extends Component {

  render() {
    const { props } = this;
    if (props.contents.length <= 0) {
      return null;
    }
    const content = props.contents[0];

    if ('code' in content) {
      return <CodeContent content={content} isParent={props.isParent} />;
    } else if ('url' in content) {
      return <UrlContent content={content} isParent={props.isParent} />;
    } else if ('text' in content) {
      return <TextContent content={content} isParent={props.isParent} />;
    } else {
      return null;
    }
  }

}
