import React from "react";
import { Association } from "~/types/metadata-update";
import { Box, Text, Button, Col, Center } from "@tlon/indigo-react";
import { Link } from "react-router-dom";

interface UnjoinedResourceProps {
  association: Association;
}

export function UnjoinedResource(props: UnjoinedResourceProps) {
  const appPath = props.association["app-path"];
  const appName = props.association["app-name"];
  const { title, description } = props.association.metadata;
  const to = `/~${appName}/join${appPath}`;
  return (
    <Center p={6}>
      <Col maxWidth="400px" p={4} border={1} borderColor="washedGray">
        <Box mb={4}>
          <Text>{title}</Text>
        </Box>
        <Box mb={4}>
          <Text color="gray">{description}</Text>
        </Box>
        <Link to={to}>
          <Button mx="auto" border>
            Join Channel
          </Button>
        </Link>
      </Col>
    </Center>
  );
}
