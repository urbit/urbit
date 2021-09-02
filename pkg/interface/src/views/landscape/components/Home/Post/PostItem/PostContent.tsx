import { Col, ColProps } from '@tlon/indigo-react';
import { Post } from '@urbit/api';
import React, { ReactElement, useCallback, useState } from 'react';
import styled, { css } from 'styled-components';
import { GraphContent } from '~/views/landscape/components/Graph/GraphContent';

type TruncateProps = ColProps & {
  truncate: boolean;
}

const TruncatedBox = styled(Col)<TruncateProps>`
  display: -webkit-box;
  -webkit-box-orient: vertical;
  ${p => p.truncate && css`
    max-height: 300px;
    mask-image: linear-gradient(to bottom, rgba(0,0,0,1) 0%, rgba(0,0,0,1) 60%, transparent 100%);
  `}
`;

interface PostContentProps {
  post: Post;
  isParent: boolean;
  isReply: boolean;
}

const PostContent = ({ post, isParent }: PostContentProps): ReactElement => {
  const [height, setHeight] = useState(0);
  const showFade = !isParent && height >= 300;

  const measuredRef = useCallback((node) => {
    if (node !== null) {
      setHeight(node.getBoundingClientRect().height);
    }
  }, []);

  return (
    <TruncatedBox
      ref={measuredRef}
      truncate={showFade}
      display="-webkit-box"
      width="90%"
      px={2}
      pb={2}
      textOverflow="ellipsis"
      overflow="hidden"
    >
      <GraphContent
        transcluded={0}
        contents={post.contents}
        showOurContact
      />
    </TruncatedBox>
  );
};

export default PostContent;

