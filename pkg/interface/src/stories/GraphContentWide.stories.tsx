import React from 'react';
import { Box } from '@tlon/indigo-react';
import { Story, Meta } from '@storybook/react';

import {
  GraphContent,
  GraphContentProps
} from '~/views/landscape/components/Graph/GraphContent';

export default {
  title: 'Graph/ContentWide',
  component: GraphContent
} as Meta;

const Template: Story<GraphContentProps> = (args) => {
  return (
    <Box
      backgroundColor="white"
      p="2"
      maxWidth="500px"
      width="100%"
      position="relative"
    >
      <GraphContent width="100%" {...args} showOurContact />
    </Box>
  );
};

export const Omnibus = Template.bind({});
Omnibus.args = {
  contents: [
    { text: 'as for @H, I literally just miscounted' },
    {
      url:
        'https://github.com/urbit/urbit/blob/master/pkg/arvo/ted/eth-watcher.hoon'
    },
    {
      text:
        'Text in landscape chats support *bolding* **italics** and ~~strikeouts~~ `code snippets`'
    },
    { mention: '~fabled-faster' },
    {
      reference: {
        graph: {
          index: '/170141184505059416342852185329797955584',
          graph: '/ship/~darrux-landes/development',
          group: '/ship/~bitbet-bolbel/urbit-community'
        }
      }
    },
    { text: ' ' },
    { mention: 'sipfyn-pidmex' },
    { text: `: you can always use a pattern like this
> blockquote demo
should not be quoted
> blockquote, then newline, then mention
` },
    { mention: '~sampel-palnet' },
    { text: '> mention inside blockquote' },
    { mention: '~sampel-palnet' }
  ]
};

