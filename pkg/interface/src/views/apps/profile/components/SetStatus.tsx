import React, {
  useState,
  useCallback,
  useEffect,
  ChangeEvent,
  useRef
} from 'react';
import {
  Row,
  Text,
  Button,
  StatelessTextInput as Input
} from '@tlon/indigo-react';
import useApi from '~/logic/api';
import { contacts, editContact } from '@urbit/api';

export function SetStatus(props: any) {
  const { contact, ship, callback } = props;
  const api = useApi();
  const inputRef = useRef(null);
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
    api.poke(contacts.editContact(ship, { status: _status }));
    inputRef.current.blur();
    if (callback) {
      callback();
    }
  };

  return (
    <Row width='100%' my={3}>
      <Input
        ref={inputRef}
        onChange={onStatusChange}
        value={_status}
        autocomplete='off'
        width='75%'
        mr={2}
        onKeyPress={(evt) => {
          if (evt.key === 'Enter') {
            editStatus();
          }
        }}
      />
      <Button primary color='white' ml={2} width='25%' onClick={editStatus}>
        Set Status
      </Button>
    </Row>
  );
}
