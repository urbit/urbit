import React from "react";
import { Text, Center, LoadingSpinner } from "@tlon/indigo-react";

import { Body } from "./Body";

interface LoadingProps {
  text?: string;
}
export function Loading({ text }: LoadingProps) {
  return (
    <Body>
      <Center height="100%">
        <LoadingSpinner />
        {!!text && <Text>{text}</Text>}
      </Center>
    </Body>
  );
}
