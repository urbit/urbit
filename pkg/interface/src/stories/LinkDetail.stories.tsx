import React from 'react';
import { Meta } from '@storybook/react';
import { withDesign } from 'storybook-addon-designs';

import { Box } from '@tlon/indigo-react';
import { LinkDetail } from '~/views/apps/links/components/LinkDetail';
import BigIntOrderedMap from '@urbit/api/lib/BigIntOrderedMap';
import { GraphNode } from '@urbit/api';
import useMetadataState from '~/logic/state/metadata';
import { makeComment } from '~/logic/lib/fixtures';

const HOUR = 60*60 * 1000;

export default {
  title: 'Collections/LinkDetail',
  component: LinkDetail,
  decorators: [withDesign]
} as Meta;

const nodeIndex = '/170141184504850861030994857749504231211';
const node = {
  post: {
    index: '/170141184504850861030994857749504231211',
    author: 'fabled-faster',
    'time-sent': 1609969377513,
    signatures: [],
    contents: [
      { text: 'IMG_20200827_150753' },
      {
        url:
          'https://fabled-faster.nyc3.digitaloceanspaces.com/fabled-faster/2021.1.06..21.42.48-structure-0001.png'
      }
    ],
    hash: null
  },
  children: new BigIntOrderedMap<GraphNode>().gas([
    makeComment(
      'ridlur-figbud',
      Date.now() - 4*HOUR,
      nodeIndex,
      [{ text: 'Beautiful' }]
    ),
    makeComment(
      'roslet-tanner',
      Date.now() - 3*HOUR,
      nodeIndex,
      [{ text: 'where did you find this?' }]
    ),

    makeComment(
      'fabled-faster',
      Date.now() - 2*HOUR,
      nodeIndex,
      [{ text: 'I dont\'t remember lol' }]
    )
  ])
};

const twitterNode = {
  post: {
    index: '/170141184504850861030994857749504231212',
    author: 'fabled-faster',
    'time-sent': 1609969377513,
    signatures: [],
    contents: [
      { text: 'LindyMan' },
      {
        url: 'https://twitter.com/PaulSkallas/status/1388896550198317056'
      }
    ],
    hash: null
  },
  children: new BigIntOrderedMap<GraphNode>().gas([
    makeComment(
      'ridlur-figbud',
      Date.now() - 4*HOUR,
      nodeIndex,
      [{ text: 'Beautiful' }]
    ),
    makeComment(
      'roslet-tanner',
      Date.now() - 3*HOUR,
      nodeIndex,
      [{ text: 'where did you find this?' }]
    ),

    makeComment(
      'fabled-faster',
      Date.now() - 2*HOUR,
      nodeIndex,
      [{ text: 'I dont\'t remember lol' }]
    )
  ])
};

export const Image = () => {
  const association = useMetadataState(
    s => s.associations.graph['/ship/~bitbet-bolbel/links']
  );
  return (
    <Box width="100%" height="100%" p="1" backgroundColor="white">
      <LinkDetail
        baseUrl="/"
        node={node}
        association={association}
      />
    </Box>
  );
};

export const Twitter = () => {
  const association = useMetadataState(
    s => s.associations.graph['/ship/~bitbet-bolbel/links']
  );
  return (
    <Box height="100%" width="100%" border="1" borderColor="lightGray" maxWidth="1166px" backgroundColor="white">
      <LinkDetail
        baseUrl="/"
        node={twitterNode}
        association={association}
      />
    </Box>
  );
};
Image.parameters = {
  design: {
    type: 'figma',
    url:
      'https://www.figma.com/file/ovD1mlsYDa0agyYTdvCmGr/Landscape?node-id=8303%3A591'
  }
};
