import React, {
  useState
} from 'react';
import { Button, Text, Box, Row, BaseTextArea } from '@tlon/indigo-react';
import tokenizeMessage from '~/logic/lib/tokenizeMessage';
import { resourceFromPath } from '~/logic/lib/group';
import { createPost } from '~/logic/api/graph';


export function PostInput(props) {
  const { api, graphPath } = props;
  const [disabled, setDisabled] = useState(false);
  const [postContent, setPostContent] = useState('');

  const sendPost = () => {
    if (!graphPath) {
      console.error("graphPath is undefined, cannot post");
      return;
    }

    setDisabled(true);
    const post = createPost(tokenizeMessage(postContent));
    const resource = resourceFromPath(graphPath);

    api.graph.addPost(
      resource.ship,
      resource.name,
      post
    ).then(() => {
      setDisabled(false);
      setPostContent('');
    });
  };

  return (
    <Box
      mb="3"
      width="100%"
      height="96px"
      borderRadius="2"
      border={1}
      borderColor="washedGray">
      <BaseTextArea
        p={2}
        backgroundColor="transparent"
        width="100%"
        color="black"
        fontSize={1}
        height="62px"
        lineNumber={3}
        style={{
          resize: 'none',
        }}
        placeholder="What's on your mind?"
        spellCheck="false"
        value={postContent}
        onChange={e => setPostContent(e.target.value)}
      />
      <Row
        borderTop={1}
        borderTopColor="washedGray"
        width="100%"
        height="32px"
        pl="2"
        display="flex"
        justifyContent="space-between"
        alignItems="center">
        <Box></Box>
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


