import React from 'react';
import { Story, Meta } from '@storybook/react';

import { Box } from '@tlon/indigo-react';
import {
  GraphContent,
  GraphContentProps
} from '~/views/landscape/components/Graph/GraphContent';

export default {
  title: 'Graph/ContentTall',
  component: GraphContent
} as Meta;

const fakeApi = {} as any;

const Template: Story<GraphContentProps> = args => (
  <Box
    maxWidth="500px"
    backgroundColor="white"
    overflowY="auto"
    width="100%"
    height="100%"
  >
    <GraphContent
      tall
      m="3"
      maxWidth="100%"
      {...args}
      api={fakeApi}
      showOurContact
    />
  </Box>
);

export const Omnibus = Template.bind({});
Omnibus.args = {
  tall: true,
  contents: [
    {
      text: `# Structure of Document

Document is organized by Symptom, Diagnosis and Mitigation. Symptoms are user-level difficulties, problems, etc. this section also includes the differential diagnosis (ie, is a ship unavailable because it’s down or because eyre is not responding). Diagnosis is the root cause of the symptom. Mitigation includes steps that can be taken to fix the user’s problem. It also will define who can take certain mitigation steps.

There are presumed to be 3 roles of support:
- **Kenny**: has normal web access but no GCP Permissions
- **Jose**: Has GCP Permissions and can perform common procedures using the ylem tooling

## Second Heading [and a link](https://urbit.org)

This demonstration text includes an inline mention here
`
    },
    { mention: '~hastuc-dibtux' },
    {
      text: `
to demonstrate text reflowing around inline elements. However, several elements do not flow inline, for example links.
`
    },
    { url: 'https://www.youtube.com/watch?v=M04AKTCDavc&t=1s' },
    {
      text: 'Links can also be images, where they\'ll be embedded with the content'
    },
    { url: 'http://media.urbit.org/site/sea30-poster.jpg' },
    {
      text: `
A link should stand alone to allow for the expansion of embedded content. Another element that does not flow inline is the permalink`
    },
    {
      reference: {
        graph: {
          index: '/170141184505059416342852185329797955584',
          graph: '/ship/~darrux-landes/development',
          group: '/ship/~bitbet-bolbel/urbit-community'
        }
      }
    },
    {
      text: `
A permalink does not flow inline to ensure that it can be embedded in the flow of the content. Also available in the tall rendering mode are unordered and ordered lists.

### TODO (heading 3)
1. Write nock
2. Write hoon
3. Write arvo
4. Write landscape
5. ???
6. Profile

As well as tables, which are written in the standard markdown style.

| Key           | Value         | 
|---------------|---------------|
| Nock          | 4K            |
| Hoon          | 140K          |
| Arvo          | 420K          |



GraphContentTall also supports inline links, like [this](https://tlon.io). These can be used if the user does not wish to highlight the linked content, for example as a footnote. Another element type is the 
>blockquote
>which renders like this
>it's created by prefixing the line with a \`>\`

Additionally, GraphContent also supports code rendering \`inline code rendering\` and block code rendering, like so
\`\`\`
function fibonacci(num: number) {
  if(num < 2) {
    return 1;
  } else {
    return fibonacci(num - 1) + fibonacci(num - 2);
  }
}
\`\`\`


`
    }
  ]
};

