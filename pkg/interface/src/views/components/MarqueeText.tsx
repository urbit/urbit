import React from 'react';
import styled from 'styled-components';
import { Box, Text, Row } from '@tlon/indigo-react';
import { PropFunc } from '~/types';

const MarqueeBox = styled(Box)<{ length: number; enable: boolean; }>`
  ${(p) => {
    if(p.length < 60) {
      return '';
    }
    return `animation: marquee ${Math.round(p.length * 0.1)}s linear infinite;`;
  }}

  position: absolute;
  overflow: hidden;
  display: flex;
  align-items: center;
  height: 100%;
  @keyframes marquee {
    0% {
      transform: translate(0%, 0);
    }
    30% {
      transform: translate(0%, 0);
    }
    100% {
      transform: translate(-100%, 0);
    }
  }
`;

  export const MarqueeText = (props: PropFunc<typeof Text> & { children: string; }) => (
  <Row justifyContent="center" height="100%" width="100%" position="relative" overflow="hidden">
    <MarqueeBox enable={props.children.length > 60} length={props.children.length}>
      <Text whiteSpace="pre" {...props} />
    </MarqueeBox>
  </Row>
);
