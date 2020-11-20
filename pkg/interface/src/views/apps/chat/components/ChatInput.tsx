import React, { Component } from 'react';
import ChatEditor from './chat-editor';
import { S3Upload } from '~/views/components/s3-upload' ;
import { uxToHex } from '~/logic/lib/util';
import { Sigil } from '~/logic/lib/sigil';
import tokenizeMessage, { isUrl } from '~/logic/lib/tokenizeMessage';
import GlobalApi from '~/logic/api/global';
import { Envelope } from '~/types/chat-update';
import { Contacts } from '~/types';
import { Row, BaseImage, Box, Icon } from '@tlon/indigo-react';

interface ChatInputProps {
  api: GlobalApi;
  numMsgs: number;
  station: any;
  ourContact: any;
  envelopes: Envelope[];
  contacts: Contacts;
  onUnmount(msg: string): void;
  s3: any;
  placeholder: string;
  message: string;
  deleteMessage(): void;
  hideAvatars: boolean;
  onPaste?(): void;
}

interface ChatInputState {
  inCodeMode: boolean;
  submitFocus: boolean;
  uploadingPaste: boolean;
}

export default class ChatInput extends Component<ChatInputProps, ChatInputState> {
  public s3Uploader: React.RefObject<S3Upload>;
  private chatEditor: React.RefObject<ChatEditor>;

  constructor(props) {
    super(props);

    this.state = {
      inCodeMode: false,
      submitFocus: false,
      uploadingPaste: false
    };

    this.s3Uploader = React.createRef();
    this.chatEditor = React.createRef();

    this.submit = this.submit.bind(this);
    this.toggleCode = this.toggleCode.bind(this);
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
    if (this.state.uploadingPaste) {
      this.chatEditor.current.editor.setValue(url);
      this.setState({ uploadingPaste: false });
    } else {
      props.api.chat.message(
        props.station,
        `~${window.ship}`,
        Date.now(),
        { url }
      );
    }
  }

  uploadError(error) {
    //  no-op for now
  }

  readyToUpload(): boolean {
    return Boolean(this.s3Uploader.current?.inputRef.current);
  }

  onPaste(codemirrorInstance, event: ClipboardEvent) {
    if (!event.clipboardData || !event.clipboardData.files.length) {
      return;
    }
    this.setState({ uploadingPaste: true });
    event.preventDefault();
    event.stopPropagation();
    this.uploadFiles(event.clipboardData.files);
  }

  uploadFiles(files: FileList) {
    if (!this.readyToUpload()) {
      return;
    }
    if (!this.s3Uploader.current || !this.s3Uploader.current.inputRef.current)
return;
    this.s3Uploader.current.inputRef.current.files = files;
    const fire = document.createEvent('HTMLEvents');
    fire.initEvent('change', true, true);
    this.s3Uploader.current?.inputRef.current?.dispatchEvent(fire);
  }

  render() {
    const { props, state } = this;

    const color = props.ourContact
      ? uxToHex(props.ourContact.color) : '000000';

    const sigilClass = props.ourContact
      ? '' : 'mix-blend-diff';

    const avatar = (
        props.ourContact &&
        ((props.ourContact.avatar !== null) && !props.hideAvatars)
      )
      ? <BaseImage src={props.ourContact.avatar} height={16} width={16} className="dib" />
      : <Sigil
        ship={window.ship}
        size={16}
        color={`#${color}`}
        classes={sigilClass}
        icon
        padded
        />;

    return (
      <Row
        alignItems='center'
        position='relative'
        flexGrow='1'
        flexShrink='0'
        borderTop='1'
        borderTopColor='washedGray'
        backgroundColor='white'
        className='cf'
        zIndex='0'
      >
        <Row p='2' alignItems='center'>
          {avatar}
        </Row>
        <ChatEditor
          ref={this.chatEditor}
          inCodeMode={state.inCodeMode}
          submit={this.submit}
          onUnmount={props.onUnmount}
          message={props.message}
          onPaste={this.onPaste.bind(this)}
          placeholder='Message...'
        />
        <Box
          mx='2'
          flexShrink='0'
          height='16px'
          width='16px'
          flexBasis='16px'
        >
          <S3Upload
            ref={this.s3Uploader}
            configuration={props.s3.configuration}
            credentials={props.s3.credentials}
            uploadSuccess={this.uploadSuccess.bind(this)}
            uploadError={this.uploadError.bind(this)}
            accept="*"
          >
            <Icon icon='Links'
              width="16"
              height="16"
            />
          </S3Upload>
        </Box>
        <Box
          mr='2'
          flexShrink='0'
          height='16px'
          width='16px'
          flexBasis='16px'
        >
          <Icon
            icon='Dojo'
            onClick={this.toggleCode}
            color={state.inCodeMode ? 'blue' : 'black'}
          />
        </Box>
      </Row>
    );
  }
}
