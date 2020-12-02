import React, { Component } from 'react';
import ChatEditor from './chat-editor';
import { IuseS3 } from '~/logic/lib/useS3';
import { uxToHex } from '~/logic/lib/util';
import { Sigil } from '~/logic/lib/sigil';
import tokenizeMessage, { isUrl } from '~/logic/lib/tokenizeMessage';
import GlobalApi from '~/logic/api/global';
import { Envelope } from '~/types/chat-update';
import { Contacts } from '~/types';
import { Row, BaseImage, Box, Icon, LoadingSpinner } from '@tlon/indigo-react';
import withS3 from '~/views/components/withS3';

type ChatInputProps = IuseS3 & {
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
}

interface ChatInputState {
  inCodeMode: boolean;
  submitFocus: boolean;
  uploadingPaste: boolean;
}

class ChatInput extends Component<ChatInputProps, ChatInputState> {
  private chatEditor: React.RefObject<ChatEditor>;

  constructor(props) {
    super(props);

    this.state = {
      inCodeMode: false,
      submitFocus: false,
      uploadingPaste: false
    };

    this.chatEditor = React.createRef();

    this.submit = this.submit.bind(this);
    this.toggleCode = this.toggleCode.bind(this);
    this.uploadSuccess = this.uploadSuccess.bind(this);
    this.uploadError = this.uploadError.bind(this);
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

  onPaste(codemirrorInstance, event: ClipboardEvent) {
    if (!event.clipboardData || !event.clipboardData.files.length) {
      return;
    }
    this.setState({ uploadingPaste: true });
    event.preventDefault();
    event.stopPropagation();
    this.uploadFiles(event.clipboardData.files);
  }

  uploadFiles(files: FileList | File[]) {
    if (!this.props.canUpload) {
      return;
    }
    Array.from(files).forEach(file => {
      this.props.uploadDefault(file)
        .then(this.uploadSuccess)
        .catch(this.uploadError);
    });
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
        flexGrow={1}
        flexShrink={0}
        borderTop={1}
        borderTopColor='washedGray'
        backgroundColor='white'
        className='cf'
        zIndex={0}
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
          mx={2}
          flexShrink={0}
          height='16px'
          width='16px'
          flexBasis='16px'
        >
          {this.props.canUpload
            ? this.props.uploading
              ? <LoadingSpinner />
              : <Icon icon='Links'
                width="16"
                height="16"
                onClick={() => this.props.promptUpload().then(this.uploadSuccess)}
              />
            : null
          }
        </Box>
        <Box
          mr={2}
          flexShrink={0}
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

export default withS3(ChatInput, {accept: 'image/*'});