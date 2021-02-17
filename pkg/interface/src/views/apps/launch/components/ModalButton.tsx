import React  from "react"
import { Box, Button, Icon, Text } from "@tlon/indigo-react"
import {useModal} from "~/logic/lib/useModal";

const ModalButton = (props) => {
  const {
    children,
    icon,
    text,
    bg,
    color,
    ...rest
  } = props;
  const { modal, showModal } = useModal({ modal: props.children });


  return (
    <>
      {modal}
      <Button
        onClick={showModal}
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
      </Button>
    </>
  );
}

export default ModalButton;
