import { BaseImage, Box, Icon, LoadingSpinner, Row } from '@tlon/indigo-react';
import { Contact, Content } from '@urbit/api';
import React, { Component, ReactNode } from 'react';
import GlobalApi from '~/logic/api/global';
import { Sigil } from '~/logic/lib/sigil';
import tokenizeMessage from '~/logic/lib/tokenizeMessage';
import { IuseStorage } from '~/logic/lib/useStorage';
import { MOBILE_BROWSER_REGEX, uxToHex } from '~/logic/lib/util';
import { withLocalState } from '~/logic/state/local';
import withStorage from '~/views/components/withStorage';
import ChatEditor from './ChatEditor';

type ChatInputProps = IuseStorage & {
  api: GlobalApi;
  ourContact?: Contact;
  onUnmount(msg: string): void;
  placeholder: string;
  message: string;
  deleteMessage(): void;
  hideAvatars: boolean;
  onSubmit: (contents: Content[]) => void;
  children?: ReactNode;
};

interface ChatInputState {
  inCodeMode: boolean;
  submitFocus: boolean;
  uploadingPaste: boolean;
  currentInput: string;
}

export class ChatInput extends Component<ChatInputProps, ChatInputState> {
  private chatEditor: React.RefObject<ChatEditor>;

  constructor(props) {
    super(props);

    this.state = {
      inCodeMode: false,
      submitFocus: false,
      uploadingPaste: false,
      currentInput: props.message
    };

    this.chatEditor = React.createRef();

    this.submit = this.submit.bind(this);
    this.toggleCode = this.toggleCode.bind(this);
    this.uploadSuccess = this.uploadSuccess.bind(this);
    this.uploadError = this.uploadError.bind(this);
    this.eventHandler = this.eventHandler.bind(this);
  }

  toggleCode() {
    this.setState({
      inCodeMode: !this.state.inCodeMode
    });
  }

  async submit(text) {
    const { props, state } = this;
    const { onSubmit, api } = this.props;
    this.setState({
      inCodeMode: false
    });
    props.deleteMessage();
    if(state.inCodeMode) {
      const output = await api.graph.eval(text) as string[];
      onSubmit([{ code: { output, expression: text } }]);
    } else {
      onSubmit(tokenizeMessage(text));
    }
    this.chatEditor.current.editor.focus();
    this.setState({ currentInput: '' });
  }

  uploadSuccess(url: string) {
    const { props } = this;
    if (this.state.uploadingPaste) {
      this.chatEditor.current.editor.setValue(url);
      this.setState({ uploadingPaste: false });
    } else {
      props.onSubmit([{ url }]);
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

  eventHandler(value) {
    this.setState({ currentInput: value });
  }

  render() {
    const { props, state } = this;

    const color = props.ourContact ? uxToHex(props.ourContact.color) : '000000';

    const sigilClass = props.ourContact ? '' : 'mix-blend-diff';

    const avatar =
      props.ourContact && props.ourContact?.avatar && !props.hideAvatars ? (
        <BaseImage
          flexShrink={0}
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
        borderTopColor='lightGray'
        backgroundColor='white'
        className='cf'
        zIndex={0}
      >
        <Row p='12px 4px 12px 12px' flexShrink={0} alignItems='center'>
          {avatar}
        </Row>
        <ChatEditor
          ref={this.chatEditor}
          inCodeMode={state.inCodeMode}
          submit={this.submit}
          onUnmount={props.onUnmount}
          message={props.message}
          onPaste={this.onPaste.bind(this)}
          changeEvent={this.eventHandler}
          placeholder='Message...'
        />
        <Box
          mx='12px'
          mr={this.props.canUpload ? '12px' : 3}
          flexShrink={0}
          height='16px'
          width='16px'
          flexBasis='16px'
        >
          <Icon
            icon='Dojo'
            cursor='pointer'
            onClick={this.toggleCode}
            color={state.inCodeMode ? 'blue' : 'black'}
          />
        </Box>
        {this.props.canUpload ? (
          <Box
            ml='12px'
            mr={3}
            flexShrink={0}
            height='16px'
            width='16px'
            flexBasis='16px'
          >
            {this.props.uploading ? (
              <LoadingSpinner />
            ) : (
              <Icon
                icon='Attachment'
                cursor='pointer'
                width='16'
                height='16'
                onClick={() =>
                  this.props.promptUpload().then(this.uploadSuccess)
                }
              />
            )}
          </Box>
        ) : null}
        {MOBILE_BROWSER_REGEX.test(navigator.userAgent) ?
          <Box
            ml={2}
            mr="12px"
            flexShrink={0}
            display="flex"
            justifyContent="center"
            alignItems="center"
            width="24px"
            height="24px"
            borderRadius="50%"
            backgroundColor={state.currentInput !== '' ? 'blue' : 'gray'}
            cursor={state.currentInput !== '' ? 'pointer' : 'default'}
            onClick={() => this.chatEditor.current.submit()}
          >
            <Icon icon="ArrowEast" color="white" />
          </Box>
          : null}
      </Row>
    );
  }
}

// @ts-ignore withLocalState prop passing weirdness
export default withLocalState<Omit<ChatInputProps, keyof IuseStorage>, 'hideAvatars', ChatInput>(
  // @ts-ignore withLocalState prop passing weirdness
  withStorage<ChatInputProps, ChatInput>(ChatInput, { accept: 'image/*' }),
  ['hideAvatars']
);
