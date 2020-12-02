import React, { useState, useEffect } from "react"
import { Box, Button, Icon, Text } from "@tlon/indigo-react"
import { NewGroup } from "~/views/landscape/components/NewGroup";
import { JoinGroup } from "~/views/landscape/components/JoinGroup";

const ModalButton = (props) => {
  const {
    childen,
    icon,
    text,
    bg,
    color,
    ...rest
  } = props;
  const [modalShown, setModalShown] = useState(false);

  const handleKeyDown = (event) => {
    if (event.key === 'Escape') {
      setModalShown(false);
    }
  }

  useEffect(() => {
    window.addEventListener('keydown', handleKeyDown);

    return () => {
      window.removeEventListener('keydown', handleKeyDown);
    };
  }, [modalShown]);

  return (
    <>
      {modalShown && (
        <Box
          backgroundColor='scales.black30'
          left="0px"
          top="0px"
          width="100%"
          height="100%"
          zIndex={4}
          position="fixed"
          display="flex"
          justifyContent="center"
          alignItems="center"
          onClick={() => setModalShown(false)}
        >
          <Box
            maxWidth="500px"
            width="100%"
            bg="white"
            borderRadius={2}
            border={[0, 1]}
            borderColor={["washedGray", "washedGray"]}
            onClick={e => e.stopPropagation()}
            display="flex"
            alignItems="stretch"
            flexDirection="column"
          >
            {props.children}
          </Box>
        </Box>
      )}
      <Box
        onClick={() => setModalShown(true)}
        display="flex"
        alignItems="center"
        cursor="pointer"
        bg={bg}
        p={2}
        borderRadius={2}
        boxShadow="0 0 0px 1px inset"
        color="scales.black20"
        {...rest}
      >
        <Icon icon={props.icon} mr={2} color={color}></Icon><Text color={color}>{props.text}</Text>
      </Box>
    </>
  );
}

export default ModalButton;