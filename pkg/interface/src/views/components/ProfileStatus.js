import React, {
  useState,
  useCallback,
  useEffect
} from 'react';

import {
  Row,
  Button,
  StatelessTextInput as Input,
} from "@tlon/indigo-react";


export const ProfileStatus = (props) => {
  const { contact, ship, api, callback } = props;
  const [_status, setStatus] = useState('');
  const onStatusChange = useCallback(
    (e) => {
      setStatus(e.target.value);
    },
    [setStatus]
  );

  useEffect(() => {
    setStatus(!!contact ? contact.status : '');
  }, [contact]);

  const editStatus = () => {
    api.contacts.edit(ship, {status: _status});

    if (callback) {
      callback();
    }
  };

  return (
    <Row width="100%" mt={3} mr={3} display="block">
      <Input
        onChange={onStatusChange}
        value={_status}
        autocomplete="off"
        width="100%"
        placeholder="Set Status"
        onKeyPress={(evt) => {
          if (evt.key === 'Enter') {
            editStatus();
          }
        }}
        onBlur={() => {
          editStatus();
        }}
      />
    </Row>
  );
};
