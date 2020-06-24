import React, { Component } from 'react';


export default class CodeContent extends Component {

  render() {
    const { props } = this;
    const font = !!props.isParent ? "f6" : "f7";

    const content = props.content;

    const outputElement =
      (Boolean(content.code.output) &&
       content.code.output.length && content.code.output.length > 0) ?
      (
        <pre className={`${font} clamp-attachment pa1 mt0 mb0 b--gray4 b--gray1-d bl br bb`}>
          {content.code.output[0].join('\n')}
        </pre>
      ) : null;
    
    return (
      <div className="mv2">
        <pre className={`${font} clamp-attachment pa1 mt0 mb0 bg-light-gray b--gray4 b--gray1-d ba`}>
          {content.code.expression}
        </pre>
        {outputElement}
      </div>
    );
  }
}
