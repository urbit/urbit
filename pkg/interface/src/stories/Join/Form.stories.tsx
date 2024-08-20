import React from "react";
import { Story, Meta } from "@storybook/react";
import { Box } from "@tlon/indigo-react";

import { Join, JoinProps } from "~/views/landscape/components/Join/Join";
import { withDesign } from "storybook-addon-designs";

export default {
  title: "Join/Form",
  component: Join,
  decorators: [withDesign],
} as Meta;

const Template: Story<JoinProps> = (args) => (
  <Box backgroundColor="white" p="2" width="fit-content">
    <Join {...args} />
  </Box>
);

export const Prompt = Template.bind({});

Prompt.args = {
  desc: {
    kind: "groups",
    group: "/ship/~bitbet-bolbel/urbit-community",
  },
};

export const WithPreview = Template.bind({});

WithPreview.args = {
  desc: {
    kind: "groups",
    group: "/ship/~bollug-worlus/urbit-index",
  },
};

WithPreview.parameters = {
  design: {
    type: "figma",
    url:
      "https://www.figma.com/file/VxNYyFRnj8ZnqWG54VbVRM/Landscape-Baikal?node-id=1795%3A27718",
  },
};

export const ProgressStart = Template.bind({});

ProgressStart.args = {
  desc: {
    kind: "groups",
    group: "/ship/~bollug-worlus/urbit-index-start",
  },
};

export const ProgressMetadata = Template.bind({});

ProgressMetadata.args = {
  desc: {
    kind: "groups",
    group: "/ship/~bollug-worlus/urbit-index-metadata",
  },
};

export const Finished = Template.bind({});

Finished.args = {
  desc: {
    kind: "groups",
    group: "/ship/~bollug-worlus/urbit-index-done",
  },
};

export const Error = Template.bind({});

Error.args = {
  desc: {
    kind: "groups",
    group: "/ship/~bollug-worlus/urbit-index-error",
  },
};
