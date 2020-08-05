import React from "react";

import { Box, Col, Center, Icon } from "@tlon/indigo-react";

import { Sigil } from "../../lib/sigil";

import Settings from "./components/settings";

export default function ProfileScreen(props: any) {
  const { ship, dark } = props;
  return (
    <Box height="100%" px={[0,3]} pb={[0,3]} borderRadius={1}>
      <Box
        height="100%"
        width="100%"
        display="flex"
        borderRadius={1}
        bg="white"
        border={1}
        borderColor="washedGray"
      >
        <Col
          display={["none", "block"]}
          collapse
          borderRight={1}
          borderColor="washedGray"
        >
          <Box borderBottom={1} borderBottomColor="washedGray">
            <Box
              bg="black"
              borderRadius={8}
              margin={4}
              height={128}
              width={128}
              display="flex"
              justifyContent="center"
              alignItems="center"
            >
              <Sigil
                ship={`~${ship}`}
                size={80}
                color={dark ? "#FFFFFF" : "#000000"}
              />
            </Box>
          </Box>
          <Box py={4}>
            <Box
              display="flex"
              alignItems="center"
              verticalAlign="middle"
              fontSize={0}
              py={1}
              px={3}
              color="blue"
              backgroundColor="washedBlue"
            >
              <Icon mr={2} display="inline-block" icon="Circle" fill="blue" />
              Ship Settings
            </Box>
          </Box>
        </Col>
        <Box overflowY="auto" flexGrow={1}>
          <Settings {...props} />
        </Box>
      </Box>
    </Box>
  );
}
