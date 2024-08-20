import React from "react";
import { Story, Meta } from "@storybook/react";
import { Box } from "@tlon/indigo-react";

import { JoinPrompt, JoinPromptProps } from "~/views/landscape/components/Join/Join";

export default {
  title: "Join/Prompt",
  component: JoinPrompt,
} as Meta;

const Template: Story<JoinPromptProps> = (args) => (
  <Box backgroundColor="white" p="2" width="fit-content">
    <JoinPrompt {...args} />
  </Box>
);

export const Prompt = Template.bind({});

Prompt.args = {
  kind: 'groups',
}
