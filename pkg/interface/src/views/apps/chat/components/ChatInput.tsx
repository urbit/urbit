import { Box, Icon, LoadingSpinner, Row } from '@tlon/indigo-react';
import { Association, Contact, Content, evalCord, Group } from '@urbit/api';
import React, { FC, PropsWithChildren, useRef, useState } from 'react';
import tokenizeMessage from '~/logic/lib/tokenizeMessage';
import { IuseStorage } from '~/logic/lib/useStorage';
import { MOBILE_BROWSER_REGEX } from '~/logic/lib/util';
import { withLocalState } from '~/logic/state/local';
import ChatEditor, { CodeMirrorShim } from './ChatEditor';
import airlock from '~/logic/api';
import { ChatAvatar } from './ChatAvatar';
import { useChatStore } from './ChatPane';
import { useImperativeHandle } from 'react';
import { FileUploadSource, useFileUpload } from '~/logic/lib/useFileUpload';

type ChatInputProps = PropsWithChildren<IuseStorage & {
  hideAvatars: boolean;
  ourContact?: Contact;
  placeholder: string;
  onSubmit: (contents: Content[]) => void;
  isAdmin: boolean;
  group: Group;
  association: Association;
  chatEditor: React.RefObject<CodeMirrorShim>
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

export const ChatInput = React.forwardRef(({
  ourContact,
  hideAvatars,
  placeholder,
  onSubmit,
  isAdmin,
  group,
  association,
  chatEditor
}: ChatInputProps, ref) => {
  // const chatEditor = useRef<CodeMirrorShim>(null);
  useImperativeHandle(ref, () => chatEditor.current);
  const [inCodeMode, setInCodeMode] = useState(false);

  const {
    message,
    setMessage
  } = useChatStore();
  const { canUpload, uploading, promptUpload, onPaste } = useFileUpload({
    onSuccess: uploadSuccess
  });

  function uploadSuccess(url: string, source: FileUploadSource) {
    if (source === 'paste') {
      setMessage(url);
    } else {
      onSubmit([{ url }]);
    }
  }

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

  return (
    <InputBox>
      <Row p='12px 4px 12px 12px' flexShrink={0} alignItems='center'>
        <ChatAvatar contact={ourContact} hideAvatars={hideAvatars} />
      </Row>
      <ChatEditor
        ref={chatEditor}
        inCodeMode={inCodeMode}
        onPaste={(cm, e) => onPaste(e)}
        {...{ submit, placeholder, isAdmin, group, association }}
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
                promptUpload().then(url => uploadSuccess(url, 'direct'))
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
});

// @ts-ignore withLocalState prop passing weirdness
export default withLocalState<Omit<ChatInputProps, keyof IuseStorage>, 'hideAvatars', ChatInput>(
  ChatInput,
  ['hideAvatars']
);
