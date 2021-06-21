import { BaseTextArea, Box, Button, Icon, LoadingSpinner, Row } from '@tlon/indigo-react';
import { addPost, Association, Content, createPost, evalCord, Group, Path } from '@urbit/api';
import React, {
  ReactElement, useCallback, useState
} from 'react';
import { isChannelAdmin, isHost, isWriter, resourceFromPath } from '~/logic/lib/group';
import tokenizeMessage from '~/logic/lib/tokenizeMessage';
import useStorage from '~/logic/lib/useStorage';
import { useToggleState } from '~/logic/lib/useToggleState';
import airlock from '~/logic/api';

function canWrite(props) {
  const { group, association, vip, index } = props;
  if (vip === '') {
    return true;
  }
  if(index) {
    return true;
  }

  if(vip === 'admin-feed') {
    return isChannelAdmin(group, association.group);
  }
  if(vip === 'host-feed') {
    return isHost(association.group);
  }

  return isWriter(group, association.resource);
}

interface PostInputProps {
  association: Association;
  graphPath: Path;
  group: Group;
  index?: string;
  submitCallback?: () => void;
  vip: string;
}

const PostInput = (props: PostInputProps): ReactElement | null => {
  const { graphPath, index, submitCallback } = props;
  const graphResource = resourceFromPath(graphPath);

  const [disabled, setDisabled] = useState(false);
  const [code, toggleCode] = useToggleState(false);
  const { canUpload, promptUpload, uploading } = useStorage();
  const [postContent, setPostContent] = useState('');

  const uploadImage = useCallback(async () => {
    try {
      setDisabled(true);
      const url = await promptUpload();
      const { ship, name } = graphResource;
      await airlock.thread(addPost(ship, name, createPost(`~${window.ship}`, [{ url }], index || '')));
    } catch (e) {
      // TODO: better handling
      console.error(e);
    } finally {
      setDisabled(false);
    }
  }, [promptUpload]);

  const sendPost = async () => {
    if (!graphResource) {
      console.error('graphResource is undefined, cannot post');
      return;
    }
    let contents: Content[] = [];
    if(code) {
      const output = await airlock.thread<string[]>(evalCord(postContent));
      contents = [{ code: { output, expression: postContent } }];
    } else {
      contents = tokenizeMessage(postContent);
    }

    setDisabled(true);
    const post = createPost(`~${window.ship}`, contents, index || '');

    await airlock.thread(addPost(
      graphResource.ship,
      graphResource.name,
      post
    ));

    setDisabled(false);
    if(code) {
      toggleCode();
    }
    setPostContent('');

    if (submitCallback) {
      submitCallback();
    }
  };

  const handleKeyDown = (e) => {
    if ((e.getModifierState('Control') || e.metaKey) && e.key === 'Enter') {
      sendPost();
    }
  };

  if (!(canWrite(props))) {
    return null;
  }

  return (
    <Box
      width="100%"
      minHeight="96px"
      borderRadius={2}
      border={1}
      borderColor="lightGray"
    >
      <BaseTextArea
        p={2}
        backgroundColor="transparent"
        width="100%"
        color="black"
        fontSize={1}
        minHeight="62px"
        fontFamily={code ? 'mono' : 'sans'}
        rows={3}
        style={{
          resize: 'vertical'
        }}
        placeholder={code ? '(add 2 2)' : 'What\'s on your mind?'}
        spellCheck="false"
        value={postContent}
        onChange={e => setPostContent(e.target.value)}
        onKeyDown={handleKeyDown}
      />
      <Row
        borderTop={1}
        borderTopColor="lightGray"
        width="100%"
        height="32px"
        pl={2}
        display="flex"
        justifyContent="space-between"
        alignItems="center"
      >
        <Row>
          {false && (
            <Box mr={2} flexShrink={0} height='16px' width='16px' flexBasis='16px'>
              <Icon
                icon='Dojo'
                onClick={toggleCode}
                color={code ? 'blue' : 'black'}
              />
            </Box>
          )}
          { canUpload && (
            <Box mr={2} flexShrink={0} height='16px' width='16px' flexBasis='16px'>
              { uploading ? (
                <LoadingSpinner />
              ) : (
                <Icon
                  icon='Attachment'
                  width='16'
                  height='16'
                  onClick={uploadImage}
                />
              )}
            </Box>
          )}
        </Row>
        <Button
          pl={2}
          pr={2}
          height="31px"
          flexShrink={0}
          backgroundColor="transparent"
          border="none"
          disabled={!postContent || disabled}
          onClick={sendPost}
        >
          Post
        </Button>
      </Row>
    </Box>
  );
};

export default PostInput;
