import React from "react";
import { Center, LoadingSpinner } from "@tlon/indigo-react";

import { Body } from "./Body";

export function Loading() {
  return (
    <Body>
      <Center height="100%">
        <LoadingSpinner />
      </Center>
    </Body>
  );
}
