import { Box } from '@tlon/indigo-react';
import React, {
    ReactNode,
    useCallback,
    useMemo,
    useRef, useState
} from 'react';
import { PropFunc } from '~/types';
import { ModalOverlay } from '~/views/components/ModalOverlay';
import { Portal } from '~/views/components/Portal';

type ModalFunc = (dismiss: () => void) => JSX.Element;
interface UseModalProps {
  modal: JSX.Element | ModalFunc;
}

interface UseModalResult {
  modal: ReactNode;
  showModal: () => void;
}

export function useModal(props: UseModalProps & PropFunc<typeof Box>): UseModalResult {
  const { modal, ...rest } = props;
  const innerRef = useRef<HTMLElement>();
  const [modalShown, setModalShown] = useState(false);

  const dismiss = useCallback(() => {
    setModalShown(false);
  }, []);

  const showModal = useCallback(() => {
    setModalShown(true);
  }, []);

  const inner = useMemo(
    () =>
      !modalShown
        ? null
        : typeof modal === 'function'
        ? modal(dismiss)
        : modal,
    [modalShown, modal, dismiss]
  );

  const modalComponent = useMemo(
    () =>
    !inner ? null : (
      <Portal>
        <ModalOverlay
          ref={innerRef}
          maxWidth="500px"
          width="100%"
          bg="white"
          borderRadius={2}
          border={[0, 1]}
          borderColor={['washedGray', 'washedGray']}
          display="flex"
          alignItems="stretch"
          flexDirection="column"
          spacing={2}
          dismiss={dismiss}
          {...rest}
        >
          {inner}
        </ModalOverlay>
      </Portal>
      ),
    [inner, dismiss]
  );

  return {
    showModal,
    modal: modalComponent
  };
}
