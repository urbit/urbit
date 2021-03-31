import React, {
  useState,
  useCallback
} from 'react';
import { LoadingSpinner, Icon, Button, Text, Box, Row, BaseTextArea } from '@tlon/indigo-react';
import tokenizeMessage from '~/logic/lib/tokenizeMessage';
import { useToggleState } from '~/logic/lib/useToggleState';
import { createPost } from '~/logic/api/graph';
import useStorage from '~/logic/lib/useStorage';
import { resourceFromPath } from '~/logic/lib/group';


export function PostInput(props) {
  const { api, graphPath, index, submitCallback } = props;
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
      await api.graph.addPost(ship, name, createPost([{ url }]));
    } catch (e) {
      // TODO: better handling
      console.error(e);
    } finally {
      setDisabled(false);

    }

  }, [promptUpload]);

  const handleKeyDown = (e) => {
    if((e.getModifierState('Control') || e.metaKey) && e.key === 'Enter') {
      sendPost();
    }
  }

  const sendPost = async () => {
    if (!graphResource) {
      console.error("graphResource is undefined, cannot post");
      return;
    }
    let contents = [];
    if(code) {
      const output = await props.api.graph.eval(postContent);
      contents = [{ code: { output, expression: postContent } }];
    } else {
      contents = tokenizeMessage(postContent);
    }

    setDisabled(true);
    const post = createPost(contents, index || '');

    api.graph.addPost(
      graphResource.ship,
      graphResource.name,
      post
    ).then(() => {
      setDisabled(false);
      if(code) {
        toggleCode();
      }
      setPostContent('');

      if (submitCallback) {
        submitCallback();
      }
    });
  };

  return (
    <Box
      width="100%"
      height="96px"
      borderRadius="2"
      border={1}
      borderColor="lightGray">
      <BaseTextArea
        p={2}
        backgroundColor="transparent"
        width="100%"
        color="black"
        fontSize={1}
        height="62px"
        fontFamily={code ? 'mono' : 'sans'}
        lineNumber={3}
        style={{
          resize: 'none',
        }}
        placeholder={code ? "(add 2 2)" : "What's on your mind?"}
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
        pl="2"
        display="flex"
        justifyContent="space-between"
        alignItems="center">
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
                  icon='Links'
                  width='16'
                  height='16'
                  onClick={uploadImage}
                />
              )}
            </Box>
          )}
        </Row>
        <Button
          pl="2"
          pr="2"
          height="31px"
          flexShrink={0}
          backgroundColor="transparent"
          border="none"
          disabled={!postContent || disabled}
          onClick={sendPost}>
          Post
        </Button>
      </Row>
    </Box>
  );
}


