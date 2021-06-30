import { Box, Icon, LoadingSpinner, Row } from '@tlon/indigo-react';
import { Contact, Content, evalCord } from '@urbit/api';
import React, { FC, PropsWithChildren, useRef, useState } from 'react';
import tokenizeMessage from '~/logic/lib/tokenizeMessage';
import useStorage, { IuseStorage } from '~/logic/lib/useStorage';
import { MOBILE_BROWSER_REGEX } from '~/logic/lib/util';
import { withLocalState } from '~/logic/state/local';
import withStorage from '~/views/components/withStorage';
import ChatEditor, { CodeMirrorShim } from './ChatEditor';
import airlock from '~/logic/api';
import { ChatAvatar } from './ChatAvatar';
import { useChatStore } from './ChatPane';

type ChatInputProps = PropsWithChildren<IuseStorage & {
  hideAvatars: boolean;
  ourContact?: Contact;
  placeholder: string;
  onUnmount(msg: string): void;
  deleteMessage(): void;
  onSubmit: (contents: Content[]) => void;
}>;

const InputBox: FC = ({ children }) => (
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
    { children }
  </Row>
);

const IconBox = ({ children, ...props }) => (
  <Box
    ml='12px'
    mr={3}
    flexShrink={0}
    height='16px'
    width='16px'
    flexBasis='16px'
    {...props}
  >
    { children }
  </Box>
);

const MobileSubmitButton = ({ enabled, onSubmit }) => (
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
    backgroundColor={enabled ? 'blue' : 'gray'}
    cursor={enabled !== '' ? 'pointer' : 'default'}
    onClick={() => onSubmit()}
  >
    <Icon icon="ArrowEast" color="white" />
  </Box>
);

export function ChatInput({ ourContact, hideAvatars, placeholder, onSubmit, onUnmount }: ChatInputProps) {
  const chatEditor = useRef<CodeMirrorShim>(null);
  const {
    message,
    setMessage
  } = useChatStore();
  const {
    canUpload, promptUpload, uploading, uploadDefault
  } = useStorage();
  const [inCodeMode, setInCodeMode] = useState(false);
  const [uploadingPaste, setUploadingPaste] = useState(false);

  function toggleCode() {
    setInCodeMode(!inCodeMode);
  }

  async function submit() {
    const text = chatEditor.current?.getValue() || '';

    if (text === '') {
      return;
    }

    if (inCodeMode) {
      const output = await airlock.thread<string[]>(evalCord(text));
      onSubmit([{ code: { output, expression: text } }]);
    } else {
      onSubmit(tokenizeMessage(text));
    }

    setInCodeMode(false);
    setMessage('');
    chatEditor.current.focus();
  }

  function uploadSuccess(url: string) {
    if (uploadingPaste) {
      chatEditor.current.setValue(url);
      setUploadingPaste(false);
    } else {
      onSubmit([{ url }]);
    }
  }

  function onPaste(codemirrorInstance, event: React.ClipboardEvent<HTMLTextAreaElement>) {
    if (!event.clipboardData || !event.clipboardData.files.length) {
      return;
    }

    setUploadingPaste(true);
    event.preventDefault();
    event.stopPropagation();
    uploadFiles(event.clipboardData.files);
  }

  function uploadFiles(files: FileList | File[]) {
    if (!canUpload) {
      return;
    }
    Array.from(files).forEach((file) => {
      uploadDefault(file)
        .then(this.uploadSuccess)
        .catch(this.uploadError);
    });
  }

  return (
    <InputBox>
      <Row p='12px 4px 12px 12px' flexShrink={0} alignItems='center'>
        <ChatAvatar contact={ourContact} hideAvatars={hideAvatars} />
      </Row>
      <ChatEditor
        ref={chatEditor}
        inCodeMode={inCodeMode}
        submit={submit}
        onUnmount={onUnmount}
        onPaste={onPaste}
        placeholder={placeholder}
      />
      <IconBox mr={canUpload ? '12px' : 3}>
        <Icon
          icon='Dojo'
          cursor='pointer'
          onClick={toggleCode}
          color={inCodeMode ? 'blue' : 'black'}
        />
      </IconBox>
      {canUpload && (
        <IconBox>
          {uploading ? (
            <LoadingSpinner />
          ) : (
            <Icon
              icon='Attachment'
              cursor='pointer'
              width='16'
              height='16'
              onClick={() =>
                promptUpload().then(uploadSuccess)
              }
            />
          )}
        </IconBox>
      )}
      {MOBILE_BROWSER_REGEX.test(navigator.userAgent) && (
        <MobileSubmitButton
          enabled={message !== ''}
          onSubmit={submit}
        />
      )}
    </InputBox>
  );
}

// @ts-ignore withLocalState prop passing weirdness
export default withLocalState<Omit<ChatInputProps, keyof IuseStorage>, 'hideAvatars', ChatInput>(
  // @ts-ignore withLocalState prop passing weirdness
  withStorage<ChatInputProps, ChatInput>(ChatInput, { accept: 'image/*' }),
  ['hideAvatars']
);
