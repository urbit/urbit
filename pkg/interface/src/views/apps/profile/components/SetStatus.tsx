import React, {
  useState,
  useCallback,
  useEffect,
  ChangeEvent
} from 'react';

import {
  Row,
  Button,
  StatelessTextInput as Input
} from '@tlon/indigo-react';
import useApi from '~/logic/lib/useApi';
import { editContact } from '@urbit/api/contacts';

export function SetStatus(props: any) {
  const { contact, ship, callback } = props;
  const api = useApi();
  const [_status, setStatus] = useState('');
  const onStatusChange = useCallback(
    (e: ChangeEvent<HTMLInputElement>) => {
      setStatus(e.target.value);
    },
    [setStatus]
  );

  useEffect(() => {
    setStatus(contact ? contact.status : '');
  }, [contact]);

  const editStatus = () => {
    api.poke(editContact(ship, { status: _status }));
    // TODO should await promise?
    if (callback) {
      callback();
    }
  };

  return (
    <Row width="100%" my={3}>
      <Input
        onChange={onStatusChange}
        value={_status}
        autocomplete="off"
        width="75%"
        mr={2}
        onKeyPress={(evt) => {
          if (evt.key === 'Enter') {
            editStatus();
          }
        }}
      />
      <Button
        primary
        color="white"
        ml={2}
        width="25%"
        onClick={editStatus}
      >
        Set Status
      </Button>
    </Row>
  );
}

