import React from 'react';
import { Meta } from '@storybook/react';
import { withDesign } from 'storybook-addon-designs';

import { Box } from '@tlon/indigo-react';
import { LinkDetail } from '~/views/apps/links/components/LinkDetail';
import BigIntOrderedMap from '@urbit/api/lib/BigIntOrderedMap';
import { GraphNode } from '@urbit/api';
import useMetadataState from '~/logic/state/metadata';
import { makeComment } from '~/logic/lib/fixtures';
import moment from 'moment';

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
      moment().hour(12).minute(34).valueOf(),
      nodeIndex,
      [{ text: 'Beautiful' }]
    ),
    makeComment(
      'roslet-tanner',
      moment().hour(12).minute(34).valueOf(),
      nodeIndex,
      [{ text: 'where did you find this?' }]
    ),

    makeComment(
      'fabled-faster',
      moment().hour(12).minute(34).valueOf(),
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
    <Box width="1166px" p="1" backgroundColor="white">
      <LinkDetail
        baseUrl="/"
        node={node}
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
