import React, { Component } from 'react';
import ChatEditor from './chat-editor';
import { S3Upload } from '~/views/components/s3-upload'
;
import { uxToHex } from '~/logic/lib/util';
import { Sigil } from '~/logic/lib/sigil';
import tokenizeMessage, { isUrl } from '~/logic/lib/tokenizeMessage';




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
    } else if (isUrl(letter)) {
      return {
        url: letter
      };
    } else {
      return {
        text: letter
      };
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

    const messages = tokenizeMessage(text);

    props.deleteMessage();

    messages.forEach((message) => {
      if (message.length > 0) {
        message = this.getLetterType(message.join(' '));
        props.api.chat.message(
          props.station,
          `~${window.ship}`,
          Date.now(),
          message
        );
      }
    });
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

  render() {
    const { props, state } = this;

    const color = props.ownerContact
      ? uxToHex(props.ownerContact.color) : '000000';

    const sigilClass = props.ownerContact
      ? '' : 'mix-blend-diff';

    const avatar = (
        props.ownerContact &&
        ((props.ownerContact.avatar !== null) && !props.hideAvatars)
      )
      ? <img src={props.ownerContact.avatar} height={24} width={24} className="dib" />
      : <Sigil
        ship={window.ship}
        size={24}
        color={`#${color}`}
        classes={sigilClass}
        />;

    return (
      <div className={
             "pa3 cf flex black white-d bt b--gray4 b--gray1-d bg-white " +
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
        <div className="ml2 mr2"
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
            accept="image/*"
          >
            <img
              className="invert-d"
              src="/~chat/img/ImageUpload.png"
              width="16"
              height="16"
            />
          </S3Upload>
        </div>
        <div style={{
            height: '16px',
            width: '16px',
            flexBasis: 16,
            marginTop: 10
          }}>
          <img style={{
              filter: state.inCodeMode && 'invert(100%)',
              height: '14px',
              width: '14px',
            }}
            onClick={this.toggleCode}
            src="/~chat/img/CodeEval.png"
            className="contrast-10-d bg-white bg-none-d ba b--gray1-d br1" />
        </div>
      </div>
    );
  }
}
