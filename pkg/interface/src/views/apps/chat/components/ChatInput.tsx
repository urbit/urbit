import { Box, Col, Icon, LoadingSpinner, Row, Text } from '@tlon/indigo-react';
import { Contact, Content, evalCord } from '@urbit/api';
import VisibilitySensor from 'react-visibility-sensor';
import React, {
  FC,
  PropsWithChildren,
  useEffect,
  useRef,
  useState
} from 'react';
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
import { Portal } from '~/views/components/Portal';
import styled from 'styled-components';
import { useOutsideClick } from '~/logic/lib/useOutsideClick';

const FixedOverlay = styled(Col)`
  position: fixed;
  -webkit-transition: all 0.1s ease-out;
  -moz-transition: all 0.1s ease-out;
  -o-transition: all 0.1s ease-out;
  transition: all 0.1s ease-out;
`;

type ChatInputProps = PropsWithChildren<
  IuseStorage & {
    hideAvatars: boolean;
    ourContact?: Contact;
    placeholder: string;
    onSubmit: (contents: Content[]) => void;
    uploadError: string;
    setUploadError: (val: string) => void;
    handleUploadError: (err: Error) => void;
  }
>;

const InputBox: FC = ({ children }) => (
  <Row
    alignItems="center"
    position="relative"
    flexGrow={1}
    flexShrink={0}
    borderTop={1}
    borderTopColor="lightGray"
    backgroundColor="white"
    className="cf"
    zIndex={0}
  >
    {children}
  </Row>
);

const IconBox = ({ children, ...props }) => (
  <Box
    ml="12px"
    mr={3}
    flexShrink={0}
    height="16px"
    width="16px"
    flexBasis="16px"
    {...props}
  >
    {children}
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

export const ChatInput = React.forwardRef(
  (
    {
      ourContact,
      hideAvatars,
      placeholder,
      onSubmit,
      uploadError,
      setUploadError,
      handleUploadError
    }: ChatInputProps,
    ref
  ) => {
    const chatEditor = useRef<CodeMirrorShim>(null);
    useImperativeHandle(ref, () => chatEditor.current);
    const [inCodeMode, setInCodeMode] = useState(false);
    const [showPortal, setShowPortal] = useState(false);
    const [visible, setVisible] = useState(false);
    const innerRef = useRef<HTMLDivElement>(null);
    const outerRef = useRef<HTMLDivElement>(null);

    useEffect(() => {
      if (!visible) {
        setShowPortal(false);
      }
    }, [visible]);

    useOutsideClick(innerRef, () => setShowPortal(false));

    const { message, setMessage } = useChatStore();
    const { canUpload, uploading, promptUpload, onPaste } = useFileUpload({
      onSuccess: uploadSuccess,
      onError: handleUploadError
    });

    function uploadSuccess(url: string, source: FileUploadSource) {
      if (source === 'paste') {
        setMessage(url);
      } else {
        onSubmit([{ url }]);
      }
      setUploadError('');
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
      <Box ref={outerRef}>
        <VisibilitySensor active={showPortal} onChange={setVisible}>
          <InputBox>
            {showPortal && (
              <Portal>
                <FixedOverlay
                  ref={innerRef}
                  backgroundColor="white"
                  color="washedGray"
                  border={1}
                  right={25}
                  bottom={75}
                  borderRadius={2}
                  borderColor="lightGray"
                  boxShadow="0px 0px 0px 3px"
                  zIndex={3}
                  fontSize={0}
                  width="250px"
                  padding={3}
                  justifyContent="center"
                  alignItems="center"
                >
                  <Text>{uploadError}</Text>
                  <Text>Please check S3 settings.</Text>
                </FixedOverlay>
              </Portal>
            )}
            <Row p="12px 4px 12px 12px" flexShrink={0} alignItems="center">
              <ChatAvatar contact={ourContact} hideAvatars={hideAvatars} />
            </Row>
            <ChatEditor
              ref={chatEditor}
              inCodeMode={inCodeMode}
              submit={submit}
              onPaste={(cm, e) => onPaste(e)}
              placeholder={placeholder}
            />
            <IconBox mr={canUpload ? '12px' : 3}>
              <Icon
                icon="Dojo"
                cursor="pointer"
                onClick={toggleCode}
                color={inCodeMode ? 'blue' : 'black'}
              />
            </IconBox>
            {canUpload && (
              <IconBox>
                {uploadError == '' && uploading && <LoadingSpinner />}
                {uploadError !== '' && (
                  <Icon
                    icon="ExclaimationMark"
                    cursor="pointer"
                    onClick={() => setShowPortal(true)}
                  />
                )}
                {uploadError == '' && !uploading && (
                  <Icon
                    icon="Attachment"
                    cursor="pointer"
                    width="16"
                    height="16"
                    onClick={() =>
                      promptUpload(handleUploadError).then(url =>
                        uploadSuccess(url, 'direct')
                      )
                    }
                  />
                )}
              </IconBox>
            )}
            {MOBILE_BROWSER_REGEX.test(navigator.userAgent) && (
              <MobileSubmitButton enabled={message !== ''} onSubmit={submit} />
            )}
          </InputBox>
        </VisibilitySensor>
      </Box>
    );
  }
);

// @ts-ignore withLocalState prop passing weirdness
export default withLocalState<
  Omit<ChatInputProps, keyof IuseStorage>,
  'hideAvatars',
  typeof ChatInput
>(ChatInput, ['hideAvatars']);
