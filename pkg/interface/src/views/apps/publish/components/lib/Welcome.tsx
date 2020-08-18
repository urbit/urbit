import React from "react";
import { Box } from "@tlon/indigo-react";
import { useLocalStorageState } from "~/logic/lib/useLocalStorageState";

export function Welcome(props: Parameters<typeof Box>[0]) {
  const [wasWelcomed, setWasWelcomed] = useLocalStorageState(
    "urbit-publish:wasWelcomed",
    false
  );

  if (wasWelcomed) {
    return null;
  }
  return (
    <Box {...props} p={2} border={1} >
      <Box lineHeight="1.6" fontSize={0}>
        Notebooks are for longer-form writing and discussion. Each Notebook is a
        collection of Markdown-formatted notes with optional comments.
      </Box>
      <Box
        fontSize={0}
        mt={2}
        className="f8 pt2 dib pointer bb"
        onClick={() => { setWasWelcomed(true) }}
      >
        Close this
      </Box>
    </Box>
  );
}

export default Welcome;
