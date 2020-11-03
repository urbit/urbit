import React, { ReactNode } from "react";

import { Box } from "@tlon/indigo-react";

export function Body(
  props: { children: ReactNode } & Parameters<typeof Box>[0]
) {
  const { children, ...boxProps } = props;
  return (
    <Box fontSize={0} px={[0, 3]} pb={[0, 3]} height="100%" width="100%">
      <Box
        bg="white"
        height="100%"
        width="100%"
        borderRadius={2}
        border={[0, 1]}
        borderColor={["washedGray", "washedGray"]}
        {...boxProps}
      >
        {props.children}
      </Box>
    </Box>
  );
}
