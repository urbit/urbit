import React, { ReactElement } from 'react';
import _ from 'lodash';
import { Formik, Form } from 'formik';
import * as Yup from 'yup';
import { RouteComponentProps, useHistory } from 'react-router-dom';

import {
  Box,
  ManagedTextInputField as Input,
  Col,
  Text
} from '@tlon/indigo-react';
import {
  Rolodex,
  deSig,
  createManagedGraph,
  createUnmanagedGraph,
  addTag,
  resourceFromPath,
  Associations,
  Groups
} from '@urbit/api';

import GlobalApi from '~/logic/api/global';
import { AsyncButton } from '~/views/components/AsyncButton';
import { FormError } from '~/views/components/FormError';
import { stringToSymbol, parentPath } from '~/logic/lib/util';
import { useWaitForProps } from '~/logic/lib/useWaitForProps';
import { ShipSearch, shipSearchSchemaInGroup, shipSearchSchema } from '~/views/components/ShipSearch';
import { IconRadio } from '~/views/components/IconRadio';
import { ChannelWriteFieldSchema, ChannelWritePerms } from './ChannelWritePerms';
import { Workspace } from '~/types/workspace';
import useApi from '~/logic/lib/useApi';
import useGroupState from '~/logic/state/groups';

type FormSchema = {
  name: string;
  description: string;
  ships: string[];
  moduleType: 'chat' | 'publish' | 'link';
} & ChannelWriteFieldSchema;

const formSchema = (members?: string[]) => Yup.object({
  name: Yup.string(),
  description: Yup.string(),
  ships: Yup.array(Yup.string()),
  moduleType: Yup.string().required('Must choose channel type'),
  writers: members ? shipSearchSchemaInGroup(members) : shipSearchSchema,
  writePerms: Yup.string()
});

interface NewChannelProps {
  group?: string;
  workspace: Workspace;
  baseUrl: string;
}

export function NewChannel(props: NewChannelProps & RouteComponentProps): ReactElement {
  const { group, workspace } = props;
  const history = useHistory();
  const groups = useGroupState(state => state.groups);
  const api = useApi();

  const waiter = useWaitForProps(props, 5000);

  const onSubmit = async (values: FormSchema, actions) => {
    const name = (values.name) ? values.name : values.moduleType;
    const resId: string = stringToSymbol(values.name)
    + ((workspace?.type !== 'messages') ? `-${Math.floor(Math.random() * 10000)}`
    : '');
    try {
      let { description, moduleType, ships, writers } = values;
      ships = ships.filter(e => e !== '');
      if (workspace?.type === 'messages' && ships.length === 1) {
        return history.push(`/~landscape/dm/${deSig(ships[0])}`);
      }
      if (group) {
        await api.thread(createManagedGraph(
          window.ship,
          resId,
          name,
          description,
          group,
          moduleType
        ));
        const tag = {
          app: 'graph',
          resource: `/ship/~${window.ship}/${resId}`,
          tag: 'writers'
        };

        const resource = resourceFromPath(group);
        writers = _.compact(writers).map(s => `~${s}`);
        const us = `~${window.ship}`;
        if (values.writePerms === 'self') {
          await api.poke(addTag(resource, tag, [us]));
        } else if(values.writePerms === 'subset') {
          writers.push(us);
          await api.poke(addTag(resource, tag, writers));
        }
      } else {
        await api.thread(createUnmanagedGraph(
          window.ship,
          resId,
          name,
          description,
          { invite: { pending: ships.map(s => `~${deSig(s)}`) } },
          moduleType
        ));
      }

      if (!group) {
        await waiter(p => Boolean(groups?.[`/ship/~${window.ship}/${resId}`]));
      }
      actions.setStatus({ success: null });
      const resourceUrl = (location.pathname.includes("/messages")) ? "/~landscape/messages" : parentPath(location.pathname);
      history.push(
        `${resourceUrl}/resource/${moduleType}/ship/~${window.ship}/${resId}`
      );
    } catch (e) {
      console.error(e);
      actions.setStatus({ error: 'Channel creation failed' });
    }
  };

  const members = group ? Array.from(groups[group]?.members) : undefined;

  return (
    <Col overflowY="auto" p={3} backgroundColor="white">
      <Box pb='3' display={['block', 'none']} onClick={() => history.push(props.baseUrl)}>
        <Text fontSize='0' bold>{'<- Back'}</Text>
      </Box>
      <Box color="black">
        <Text fontSize={2} bold>{workspace?.type === 'messages' ? 'Direct Message' : 'New Channel'}</Text>
      </Box>
      <Formik
        validationSchema={formSchema(members)}
        initialValues={{
          moduleType: (workspace?.type === 'home') ? 'publish' : 'chat',
          name: '',
          description: '',
          group: '',
          ships: [],
          writePerms: 'everyone',
          writers: []
        }}
        onSubmit={onSubmit}
      >
      <Form>
          <Col
          maxWidth="348px"
          gapY="4"
          >
            <Col pt={4} gapY="2" display={(workspace?.type === 'messages') ? 'none' : 'flex'}>
              <Box fontSize="1" color="black" mb={2}>Channel Type</Box>
              <IconRadio
                display={!(workspace?.type === 'home') ? 'flex' : 'none'}
                icon="Chat"
                label="Chat"
                id="chat"
                name="moduleType"
              />
              <IconRadio
                icon="Publish"
                label="Notebook"
                id="publish"
                name="moduleType"
              />
              <IconRadio
                icon="Collection"
                label="Collection"
                id="link"
                name="moduleType"
              />
            </Col>
            <Input
            display={workspace?.type === 'messages' ? 'none' : 'flex'}
            id="name"
            label="Name"
            caption="Provide a name for your channel"
            placeholder="eg. My Channel"
            />
            <Input
            display={workspace?.type === 'messages' ? 'none' : 'flex'}
            id="description"
            label="Description"
            caption="What's your channel about?"
            placeholder="Channel description"
            />
            {(workspace?.type === 'home' || workspace?.type === 'messages') ? (
            <ShipSearch
            id="ships"
            label="Invitees"
            />) : (
            <ChannelWritePerms />
          )}
            <Box justifySelf="start">
              <AsyncButton
              primary
              loadingText="Creating..."
              type="submit"
              border
              >
                Create
              </AsyncButton>
            </Box>
          <FormError message="Channel creation failed" />
          </Col>
        </Form>
      </Formik>
    </Col>
  );
}
