import React, { Component } from 'react';
import ChatEditor from './chat-editor';
import { IuseStorage } from '~/logic/lib/useStorage';
import { uxToHex } from '~/logic/lib/util';
import { Sigil } from '~/logic/lib/sigil';
import { createPost } from '~/logic/api/graph';
import tokenizeMessage, { isUrl } from '~/logic/lib/tokenizeMessage';
import GlobalApi from '~/logic/api/global';
import { Envelope } from '~/types/chat-update';
import { StorageState } from '~/types';
import { Contacts, Content } from '@urbit/api';
import { Row, BaseImage, Box, Icon, LoadingSpinner } from '@tlon/indigo-react';
import withStorage from '~/views/components/withStorage';
import { withLocalState } from '~/logic/state/local';

type ChatInputProps = IuseStorage & {
  api: GlobalApi;
  numMsgs: number;
  station: unknown;
  ourContact: unknown;
  envelopes: Envelope[];
  onUnmount(msg: string): void;
  placeholder: string;
  message: string;
  deleteMessage(): void;
  hideAvatars: boolean;
};

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

  submit(text) {
    const { props, state } = this;
    const [, , ship, name] = props.station.split('/');
    if (state.inCodeMode) {
      this.setState(
        {
          inCodeMode: false
        },
        async () => {
          const output = await props.api.graph.eval(text);
          const contents: Content[] = [{ code: { output, expression: text } }];
          const post = createPost(contents);
          props.api.graph.addPost(ship, name, post);
        }
      );
      return;
    }

    const post = createPost(tokenizeMessage(text));

    props.deleteMessage();

    props.api.graph.addPost(ship, name, post);
  }

  uploadSuccess(url) {
    const { props } = this;
    if (this.state.uploadingPaste) {
      this.chatEditor.current.editor.setValue(url);
      this.setState({ uploadingPaste: false });
    } else {
      const [, , ship, name] = props.station.split('/');
      props.api.graph.addPost(ship, name, createPost([{ url }]));
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
    Array.from(files).forEach((file) => {
      this.props
        .uploadDefault(file)
        .then(this.uploadSuccess)
        .catch(this.uploadError);
    });
  }

  render() {
    const { props, state } = this;

    const color = props.ourContact ? uxToHex(props.ourContact.color) : '000000';

    const sigilClass = props.ourContact ? '' : 'mix-blend-diff';

    const avatar =
      props.ourContact && props.ourContact?.avatar && !props.hideAvatars ? (
        <BaseImage
          src={props.ourContact.avatar}
          height={24}
          width={24}
          style={{ objectFit: 'cover' }}
          borderRadius={1}
          display='inline-block'
        />
      ) : (
        <Box
          width={24}
          height={24}
          display='flex'
          justifyContent='center'
          alignItems='center'
          backgroundColor={`#${color}`}
          borderRadius={1}
        >
          <Sigil
            ship={window.ship}
            size={16}
            color={`#${color}`}
            classes={sigilClass}
            icon
            padding={2}
          />
        </Box>
      );

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
        <Row p='12px 8px 12px 12px' alignItems='center'>
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
        <Box mx={2} flexShrink={0} height='16px' width='16px' flexBasis='16px'>
          {this.props.canUpload ? (
            this.props.uploading ? (
              <LoadingSpinner />
            ) : (
              <Icon
                icon='Links'
                width='16'
                height='16'
                onClick={() =>
                  this.props.promptUpload().then(this.uploadSuccess)
                }
              />
            )
          ) : null}
        </Box>
        <Box mr={2} flexShrink={0} height='16px' width='16px' flexBasis='16px'>
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

export default withLocalState(withStorage(ChatInput, { accept: 'image/*' }), [
  'hideAvatars'
]);
