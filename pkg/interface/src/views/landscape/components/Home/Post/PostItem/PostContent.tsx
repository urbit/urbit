import { Col, ColProps } from '@tlon/indigo-react';
import { Post } from '@urbit/api';
import React, { ReactElement } from 'react';
import styled from 'styled-components';
import GlobalApi from '~/logic/api/global';
import { GraphContent } from '~/views/landscape/components/Graph/GraphContent';

type TruncateProps = ColProps & {
  truncate?: number;
}

const TruncatedBox = styled(Col)<TruncateProps>`
  display: -webkit-box;
  -webkit-line-clamp: ${p => p.truncate ?? 'unset'};
  -webkit-box-orient: vertical;
`;

interface PostContentProps {
  post: Post;
  api: GlobalApi;
  isParent: boolean;
  isReply: boolean;
}

const PostContent = (props: PostContentProps): ReactElement => {
  const { post, isParent, api } = props;

  return (
    <TruncatedBox
      display="-webkit-box"
      width="100%"
      px={2}
      pb={2}
      truncate={isParent ? null : 8}
      textOverflow="ellipsis"
      overflow="hidden"
      className='post-content'
    >
      <GraphContent
        transcluded={0}
        contents={post.contents}
        api={api}
        showOurContact
      />
    </TruncatedBox>
  );
};

export default PostContent;

