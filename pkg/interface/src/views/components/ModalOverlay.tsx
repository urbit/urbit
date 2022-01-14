import { Box } from '@tlon/indigo-react';
import React, { useCallback, useRef } from 'react';
import { PropFunc } from '~/types/util';

interface ModalOverlayProps {
  spacing?: PropFunc<typeof Box>['m'];
  dismiss: () => void;
}
type Props = ModalOverlayProps & PropFunc<typeof Box>;
export const ModalOverlay = (props: Props) => {
  const { spacing, ...rest } = props;
  const ref = useRef<HTMLElement | null>(null);
  const onClick = useCallback(
    (e: any) => {
      if (!(ref as any).current.contains(e.target)) {
        props.dismiss();
      }
      e.stopPropagation();
    },
    [props.dismiss, ref]
  );

  const onKeyDown = useCallback(
    (e: any) => {
      if (e.key === 'Escape') {
        props.dismiss();
        e.stopPropagation();
      }
    },
    [props.dismiss, ref]
  );

  return (
    <Box
      backgroundColor="scales.black20"
      left="0px"
      top="0px"
      width="100%"
      height="100%"
      position="fixed"
      display="flex"
      zIndex={100}
      justifyContent="center"
      alignItems="center"
      p={spacing}
      onClick={onClick}
      onKeyDown={onKeyDown}
    >
      <Box ref={ref} {...rest} />
    </Box>
  );
};
