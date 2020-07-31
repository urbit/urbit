import React, { Component } from 'react';
import { Sigil } from '../../../../lib/sigil';
import ChatEditor from './chat-editor';
import { S3Upload } from './s3-upload';
import { uxToHex } from '../../../../lib/util';



const URL_REGEX = new RegExp(String(/^((\w+:\/\/)[-a-zA-Z0-9:@;?&=\/%\+\.\*!'\(\),\$_\{\}\^~\[\]`#|]+)/.source));

export class ChatInput extends Component {
  constructor(props) {
    super(props);
    
    this.state = {
      inCodeMode: false,
    };

    this.submit = this.submit.bind(this);
    this.toggleCode = this.toggleCode.bind(this);
  }

  uploadSuccess(url) {
    const { props } = this;
    props.api.chat.message(
      props.station,
      `~${window.ship}`,
      Date.now(),
      { url }
    );
  }

  uploadError(error) {
    //  no-op for now
  }

  toggleCode() {
    this.setState({
      inCodeMode: !this.state.inCodeMode
    });
  }

  getLetterType(letter) {
    if (letter.startsWith('/me ')) {
      letter = letter.slice(4);
      // remove insignificant leading whitespace.
      // aces might be relevant to style.
      while (letter[0] === '\n') {
        letter = letter.slice(1);
      }

      return {
        me: letter
      };
    } else if (this.isUrl(letter)) {
      return {
        url: letter
      };
    } else {
      return {
        text: letter
      };
    }
  }

  isUrl(string) {
    try {
      return URL_REGEX.test(string);
    } catch (e) {
      return false;
    }
  }

  submit(text) {
    const { props, state } = this;
    if (state.inCodeMode) {
      this.setState({
        inCodeMode: false
      }, () => {
        props.api.chat.message(
          props.station,
          `~${window.ship}`,
          Date.now(), {
            code: {
              expression: text,
              output: undefined
            }
          }
        );
      });
      return;
    }

    let message = [];
    let isInCodeBlock = false;
    let endOfCodeBlock = false;
    text.split(/\r?\n/).forEach((line) => {
      // A line of backticks enters and exits a codeblock
      if (line.startsWith('```')) {
        // But we need to check if we've ended a codeblock
        endOfCodeBlock = isInCodeBlock;
        isInCodeBlock = (!isInCodeBlock);
      } else {
        endOfCodeBlock = false;
      }
      if (isInCodeBlock) {
        message.push(`\n${line}`);
      } else if (endOfCodeBlock) {
        message.push(`\n${line}\n`);
      } else {
        line.split(/\s/).forEach((str) => {
          if (
            (str.startsWith('`') && str !== '`')
            || (str === '`' && !isInCodeBlock)
          ) {
            isInCodeBlock = true;
          } else if (
            (str.endsWith('`') && str !== '`')
            || (str === '`' && isInCodeBlock)
          ) {
            isInCodeBlock = false;
          }
          if (this.isUrl(str) && !isInCodeBlock) {
            if (message.length > 0) {
              message = message.join(' ');
              message = this.getLetterType(message);
              props.api.chat.message(
                props.station,
                `~${window.ship}`,
                Date.now(),
                message
              );
              message = [];
            }
            const URL = this.getLetterType(str);
            props.api.chat.message(
              props.station,
              `~${window.ship}`,
              Date.now(),
              URL
            );
          } else {
            message.push(str);
          }
        });

      }

    });

    if (message.length > 0) {
      message = message.join(' ');
      message = this.getLetterType(message);
      props.api.chat.message(
        props.station,
        `~${window.ship}`,
        Date.now(),
        message
      );
      message = [];
    }
  }

  render() {
    const { props, state } = this;

    const color = props.ownerContact
      ? uxToHex(props.ownerContact.color) : '000000';

    const sigilClass = props.ownerContact
      ? '' : 'mix-blend-diff';

    const avatar = (props.ownerContact && (props.ownerContact.avatar !== null))
      ? <img src={props.ownerContact.avatar} height={24} width={24} className="dib" />
      : <Sigil
        ship={window.ship}
        size={24}
        color={`#${color}`}
        classes={sigilClass}
        />;

    return (
      <div className={
             "pa3 cf flex black white-d bt b--gray4 b--gray1-d bg-white" +
             "bg-gray0-d relative"
           }
           style={{ flexGrow: 1 }}>
        <div className="fl"
             style={{
               marginTop: 6,
               flexBasis: 24,
               height: 24
             }}>
          {avatar}
        </div>
        <ChatEditor
          inCodeMode={state.inCodeMode}
          submit={this.submit}
          onUnmount={props.onUnmount}
          message={props.message}
          placeholder='Message...' />
        <div className="ml3 mr3"
             style={{
               height: '16px',
               width: '16px',
               flexBasis: 16,
               marginTop: 10
             }}>
          <S3Upload
            configuration={props.s3.configuration}
            credentials={props.s3.credentials}
            uploadSuccess={this.uploadSuccess.bind(this)}
            uploadError={this.uploadError.bind(this)}
          />
        </div>
        <div style={{
               height: '16px',
               width: '16px',
               flexBasis: 16,
               marginTop: 10
          }}>
          <img style={{
                 filter: state.inCodeMode && 'invert(100%)',
                 height: '100%',
                 width: '100%'
               }}
               onClick={this.toggleCode}
               src="/~chat/img/CodeEval.png"
               className="contrast-10-d bg-white bg-none-d ba b--gray1-d br1" />
        </div>
      </div>
    );
  }
}
