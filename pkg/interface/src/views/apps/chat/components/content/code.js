import React, { Component } from 'react';


export default class CodeContent extends Component {

  render() {
    const { props } = this;
    const content = props.content;

    const outputElement =
      (Boolean(content.code.output) &&
       content.code.output.length && content.code.output.length > 0) ?
      (
        <pre className={`code f9 clamp-attachment pa1 mt0 mb0`}>
          {content.code.output[0].join('\n')}
        </pre>
      ) : null;

    return (
      <div className="mv2">
        <pre className={`code f9 clamp-attachment pa1 mt0 mb0`}>
          {content.code.expression}
        </pre>
        {outputElement}
      </div>
    );
  }
}
