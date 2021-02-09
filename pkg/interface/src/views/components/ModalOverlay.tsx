import React from "react";
import { Box } from "@tlon/indigo-react";
import { PropFunc } from "~/types/util";

interface ModalOverlayProps {
  spacing: PropFunc<typeof Box>["m"];
}
export const ModalOverlay = React.forwardRef(
  (props: ModalOverlayProps & PropFunc<typeof Box>, ref) => {
    const { spacing, ...rest } = props;
    return (
      <Box
        backgroundColor="scales.black20"
        left="0px"
        top="0px"
        width="100%"
        height="100%"
        zIndex={10}
        position="fixed"
        display="flex"
        justifyContent="center"
        alignItems="center"
        p={spacing}
      >
        <Box ref={ref} {...rest} />
      </Box>
    );
  }
);
