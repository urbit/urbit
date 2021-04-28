import React from 'react';
import { Col, Box } from '@tlon/indigo-react';
import { GraphContentWide } from "~/views/landscape/components/Graph/GraphContentWide";
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
      <GraphContentWide
        transcluded={0}
        post={post}
        api={api}
        showOurContact
      />
    </TruncatedBox>
  );
}

