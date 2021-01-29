import React, {
  useState,
  ReactNode,
  useCallback,
  SyntheticEvent,
  useMemo,
  useEffect,
} from "react";

import { Box } from "@tlon/indigo-react";

type ModalFunc = (dismiss: () => void) => JSX.Element;
interface UseModalProps {
  modal: JSX.Element | ModalFunc;
}

interface UseModalResult {
  modal: ReactNode;
  showModal: () => void;
}

const stopPropagation = (e: SyntheticEvent) => {
  e.stopPropagation();
};

export function useModal(props: UseModalProps): UseModalResult {
  const [modalShown, setModalShown] = useState(false);

  const dismiss = useCallback(() => {
    setModalShown(false);
  }, [setModalShown]);

  const showModal = useCallback(() => {
    setModalShown(true);
  }, [setModalShown]);

  const inner = useMemo(
    () =>
      !modalShown
        ? null
        : typeof props.modal === "function"
        ? props.modal(dismiss)
        : props.modal,
    [modalShown, props.modal, dismiss]
  );

  const handleKeyDown = useCallback(
    (event) => {
      if (event.key === "Escape") {
        dismiss();
      }
    },
    [dismiss]
  );

  useEffect(() => {
    window.addEventListener("keydown", handleKeyDown);

    return () => {
      window.removeEventListener("keydown", handleKeyDown);
    };
  }, [modalShown]);

  const modal = useMemo(
    () =>
      !inner ? null : (
        <Box
          backgroundColor="scales.black30"
          left="0px"
          top="0px"
          width="100%"
          height="100%"
          zIndex={10}
          position="fixed"
          display="flex"
          justifyContent="center"
          alignItems="center"
          onClick={dismiss}
        >
          <Box
            maxWidth="500px"
            width="100%"
            bg="white"
            borderRadius={2}
            border={[0, 1]}
            borderColor={["washedGray", "washedGray"]}
            onClick={stopPropagation}
            display="flex"
            alignItems="stretch"
            flexDirection="column"
          >
            {inner}
          </Box>
        </Box>
      ),
    [inner, dismiss]
  );

  return {
    showModal,
    modal,
  };
}
