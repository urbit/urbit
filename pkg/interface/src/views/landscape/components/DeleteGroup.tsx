import { Button, Col, Icon, Label, Row, Text } from '@tlon/indigo-react';
import { Association, deleteGroup, leaveGroup } from '@urbit/api';
import React from 'react';
import { useHistory } from 'react-router-dom';
import { resourceFromPath } from '~/logic/lib/group';
import { useModal } from '~/logic/lib/useModal';
import { StatelessAsyncButton } from '~/views/components/StatelessAsyncButton';
import airlock from '~/logic/api';

export function DeleteGroup(props: {
  owner: boolean;
  association: Association;
}) {
  const history = useHistory();
  const onDelete = async () => {
    const { ship, name } = resourceFromPath(props.association.group);
    if (props.owner) {
      const shouldDelete =
        prompt(`To confirm deleting this group, type ${name}`) === name;
      if (!shouldDelete)
return;
    }
    if(props.owner) {
      airlock.thread(deleteGroup(ship, name));
    } else {
      airlock.thread(leaveGroup(ship, name));
    }
    history.push('/');
  };

  const action = props.owner ? 'Archive' : 'Leave';
  const description = props.owner
    ? 'Permanently archive this group. (All current members will no longer see this group.)'
    : 'You can rejoin if it is an open group, or if you are reinvited';

  const icon = props.owner ? 'X' : 'LogOut';
  const { modal, showModal } = useModal({ modal:
    (dismiss: () => void) => {
      const onCancel = (e) => {
        e.stopPropagation();
        dismiss();
      };
      return (
        <Col p={4}>
          <Label>{action} Group</Label>
          <Label gray mt={2}>
            {description}
          </Label>
          <Row mt={2} justifyContent="flex-end">
            <Button onClick={onCancel}>Cancel</Button>
            <StatelessAsyncButton
              name={`delete-${props.association.group}`}
              onClick={onDelete}
              ml={2}
              destructive
              primary
            >
              {action} {`"${props.association.metadata.title}"`}
            </StatelessAsyncButton>
          </Row>
        </Col>
      );
    } });
  return (
    <Row px={3} py={1} onClick={showModal} cursor="pointer">
      {modal}
      <Icon icon={icon} color="red" mr={2} />
      <Text color="red">
        {action} group
      </Text>
    </Row>
  );
}
