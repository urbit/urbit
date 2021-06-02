import React from 'react';
import { Meta } from '@storybook/react';
import { withDesign } from 'storybook-addon-designs';

import { Col, Row } from '@tlon/indigo-react';
import { LinkBlockItem } from '~/views/apps/links/components/LinkBlockItem';

export default {
  title: 'Collections/BlockItem',
  component: LinkBlockItem,
  decorators: [withDesign]
} as Meta;

export const Image = () => (
  <Row flexWrap="wrap" m="2" width="700px" backgroundColor="white">
    <LinkBlockItem m="2" url="https://media.urbit.org/site/posts/essays/value-of-address-space-pt1.jpg" />
    <LinkBlockItem m="2" url="https://media.urbit.org/site/posts/essays/ocean.jpeg" />
    <LinkBlockItem m="2" size="512px" url="https://media.urbit.org/site/posts/essays/ocean.jpeg" />
  </Row>
);

Image.parameters = {
  design: {
    type: 'figma',
    url:
      'https://www.figma.com/file/ovD1mlsYDa0agyYTdvCmGr/Landscape?node-id=8228%3A11'
  }
};

export const Fallback = () => (
  <Col gapY="2" p="2" width="500px" backgroundColor="white">
    <LinkBlockItem url="https://www.are.na/edouard-urcades/edouard" />
    <LinkBlockItem url="https://thejaymo.net" />
  </Col>
);

Fallback.parameters = {
  design: {
    type: 'figma',
    url:
      'https://www.figma.com/file/ovD1mlsYDa0agyYTdvCmGr/Landscape?node-id=8228%3A57'
  }
};

export const Audio = () => (
  <Col gapY="2" p="2" width="500px" backgroundColor="white">
    <LinkBlockItem
      title="Artist · Track"
      url="https://rovnys-public.s3.amazonaws.com/urbit-from-the-outside-in-1.m4a"
    />
  </Col>
);

Audio.parameters = {
  design: {
    type: 'figma',
    url:
      'https://www.figma.com/file/ovD1mlsYDa0agyYTdvCmGr/Landscape?node-id=8229%3A0'
  }
};

export const Youtube = () => (
  <Col gapY="2" p="2" width="500px" backgroundColor="white">
    <LinkBlockItem
      title="Artist · Track"
      url="https://www.youtube.com/watch?v=M04AKTCDavc&t=1s"
    />
  </Col>
);

Youtube.parameters = {
  design: {
    type: 'figma',
    url:
      'https://www.figma.com/file/ovD1mlsYDa0agyYTdvCmGr/Landscape?node-id=8229%3A0'
  }
};
