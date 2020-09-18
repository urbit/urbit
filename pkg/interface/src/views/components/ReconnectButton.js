import React from "react";
import { Box, Text } from "@tlon/indigo-react";

const ReconnectBox = ({ color, children, onClick }) => (
  <Box
    ml={2}
    px={2}
    py={1}
    display="flex"
    color={color}
    bg="white"
    alignItems="center"
    border={1}
    verticalAlign="middle"
    lineHeight="0"
    borderRadius={2}
    style={{ cursor: "pointer" }}
    onClick={onClick}
  >
    <Text color={color}>{children}</Text>
  </Box>
);

const ReconnectButton = ({ connection, subscription }) => {
  const connectedStatus = connection || "connected";
  const reconnect = subscription.restart.bind(subscription);
  if (connectedStatus === "disconnected") {
    return (
      <ReconnectBox onClick={reconnect} color="red">
        Reconnect ↻
      </ReconnectBox>
    );
  } else if (connectedStatus === "reconnecting") {
    return <ReconnectBox color="yellow">Reconnecting</ReconnectBox>;
  } else {
    return null;
  }
};

export default ReconnectButton;
