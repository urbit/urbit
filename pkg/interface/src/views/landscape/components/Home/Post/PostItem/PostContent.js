import React from 'react';
import { Col, Box } from '@tlon/indigo-react';
import { GraphContent } from '~/views/landscape/components/Graph/GraphContent';
import styled from 'styled-components';

const TruncatedBox = styled(Col)`
  display: -webkit-box;
  -webkit-line-clamp: ${p => p.truncate ?? 'unset'};
  -webkit-box-orient: vertical;

`;

export function PostContent(props) {
  const { post, isParent, api, isReply } = props;

  return (
    <TruncatedBox
      display="-webkit-box"
      width="100%"
      px="2"
      pb="2"
      truncate={isParent ? null : 8}
      textOverflow="ellipsis"
      overflow="hidden"
     >
      <GraphContent
        transcluded={0}
        contents={post.contents}
        api={api}
        showOurContact
      />
    </TruncatedBox>
  );
}

