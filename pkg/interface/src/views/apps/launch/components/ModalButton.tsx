import { Button, Icon, Row, Text } from '@tlon/indigo-react';
import React from 'react';
import { useModal } from '~/logic/lib/useModal';

const ModalButton = (props) => {
  const { children, icon, text, bg, color, ...rest } = props;
  const { modal, showModal } = useModal({ modal: props.children });

  return (
    <>
      {modal}
      <Button
        onClick={showModal}
        display='flex'
        cursor='pointer'
        bg="white"
        overflow='hidden'
        border={0}
        p={0}
        borderRadius={2}
        {...rest}
      >
        <Row bg={bg} p={2} width='100%' justifyContent="start" alignItems="center">
        <Icon icon={props.icon} mr={2} color={color}></Icon>
        <Text color={color} fontWeight="medium" whiteSpace='nowrap'>
          {props.text}
        </Text></Row>
      </Button>
    </>
  );
};

export default ModalButton;
