import {
  Box,
  Col,
  ManagedTextInputField as Input,
  Text
} from '@tlon/indigo-react';
import { addTag, createManagedGraph, createUnmanagedGraph, resourceFromPath } from '@urbit/api';
import { Form, Formik } from 'formik';
import _ from 'lodash';
import React, { ReactElement } from 'react';
import { useHistory, useRouteMatch } from 'react-router-dom';
import * as Yup from 'yup';
import { useWaitForProps } from '~/logic/lib/useWaitForProps';
import { deSig, parentPath, stringToSymbol } from '~/logic/lib/util';
import useGroupState from '~/logic/state/group';
import { PropFunc } from '~/types/util';
import { Workspace } from '~/types/workspace';
import { AsyncButton } from '~/views/components/AsyncButton';
import { FormError } from '~/views/components/FormError';
import { IconRadio } from '~/views/components/IconRadio';
import { ShipSearch, shipSearchSchema, shipSearchSchemaInGroup } from '~/views/components/ShipSearch';
import { ChannelWriteFieldSchema, ChannelWritePerms } from './ChannelWritePerms';
import airlock from '~/logic/api';

type FormSchema = {
  name: string;
  description: string;
  ships: string[];
  moduleType: 'chat' | 'publish' | 'link';
} & ChannelWriteFieldSchema;

const formSchema = (members?: string[]) =>
  Yup.object({
    name: Yup.string(),
    description: Yup.string(),
    ships: Yup.array(Yup.string()),
    moduleType: Yup.string().required('Must choose channel type'),
    writers: members ? shipSearchSchemaInGroup(members) : shipSearchSchema,
    writePerms: Yup.string()
  });

type NewChannelProps = {
  group?: string;
  workspace: Workspace;
  baseUrl?: string;
  existingMembers?: string[];
} & PropFunc<typeof Col>;

export function NewChannel(props: NewChannelProps): ReactElement {
  const history = useHistory();
  const match = useRouteMatch();
  const { group, workspace, existingMembers, ...rest } = props;
  const groups = useGroupState(state => state.groups);
  const waiter = useWaitForProps({ groups }, 5000);

  const channelName = (values) => {
    if (values.name)
      return values.name;
    if (!values.name && workspace?.type === 'messages') {
      const joinedShips = values.ships
        .filter(Boolean)
        .map(ship => `~${deSig(ship)}`)
        .join(', ')
        .concat(`, ~${deSig(window.ship)}`);
      return joinedShips;
    }
    return values.moduleType;
  };

  const onSubmit = async (values: FormSchema, actions) => {
    const name = channelName(values);
    const resId: string =
      await stringToSymbol(values.name) +
      (workspace?.type !== 'messages'
        ? `-${Math.floor(Math.random() * 10000)}`
        : '');
    try {
      let { description, moduleType, ships, writers } = values;
      ships = ships.filter(e => e !== '');
      if (workspace?.type === 'messages' && ships.length === 1) {
        return history.push(`/~landscape/messages/dm/~${deSig(ships[0])}`);
      }
      if (group) {
        await airlock.thread(createManagedGraph(
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
          await airlock.poke(addTag(resource, tag, [us]));
        } else if (values.writePerms === 'subset') {
          writers.push(us);
          await airlock.poke(addTag(resource, tag, writers));
        }
      } else {
        await airlock.thread(createUnmanagedGraph(
          window.ship,
          resId,
          name,
          description,
          { invite: { pending: ships.map(s => `~${deSig(s)}`) } },
          moduleType
        ));
      }

      if (!group) {
        await waiter(p =>
          Boolean(p.groups?.[`/ship/~${window.ship}/${resId}`])
        );
      }
      actions.setStatus({ success: null });
      const resourceUrl = match.url.includes('/messages')
        ? '/~landscape/messages'
        : parentPath(match.path);
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
    <Col
      overflowY='auto'
      p={3}
      backgroundColor='white'
      {...rest}
    >
      <Box
        pb={3}
        display={workspace?.type === 'messages' ? 'none' : ['block', 'none']}
        onClick={() => history.push(props?.baseUrl ?? '/')}
      >
        <Text>{'<- Back'}</Text>
      </Box>
      <Box>
        <Text fontSize={2} bold>
          {workspace?.type === 'messages' && existingMembers ? 'New Group ' : null}
          {workspace?.type === 'messages' ? 'Direct Message' : 'New Channel'}
        </Text>
      </Box>
      <Formik
        validationSchema={formSchema(members)}
        initialValues={{
          moduleType: workspace?.type === 'home' ? 'publish' : 'chat',
          name: '',
          description: '',
          group: '',
          ships: existingMembers ? [...existingMembers] : [],
          writePerms: 'everyone',
          writers: []
        }}
        onSubmit={onSubmit}
      >
      <Form>
          <Col
          maxWidth="348px"
          gapY={4}
          >
            <Col pt={4} gapY={2} display={(workspace?.type === 'messages') ? 'none' : 'flex'}>
              <Box fontSize={1} color="black" mb={2}>Channel Type</Box>
              <IconRadio
                display={!(workspace?.type === 'home') ? 'flex' : 'none'}
                icon='Chat'
                label='Chat'
                id='chat'
                name='moduleType'
              />
              <IconRadio
                icon="Publish"
                label="Notebook"
                id="publish"
                name="moduleType"
              />
              <IconRadio
                icon='Collection'
                label='Collection'
                id='link'
                name='moduleType'
              />
            </Col>
            <Input
              display={workspace?.type === 'messages' ? 'none' : 'flex'}
              id='name'
              label='Name'
              caption='Provide a name for your channel'
              placeholder='eg. My Channel'
            />
            <Input
              display={workspace?.type === 'messages' ? 'none' : 'flex'}
              id='description'
              label='Description'
              caption="What's your channel about?"
              placeholder='Channel description'
            />
            {workspace?.type === 'home' || workspace?.type === 'messages' ? (
              <ShipSearch id='ships' label='Invitees' />
            ) : (
              <ChannelWritePerms />
            )}
            <Box justifySelf='start'>
              <AsyncButton
                primary
                loadingText='Creating...'
                type='submit'
                border
              >
                Create
              </AsyncButton>
            </Box>
            <FormError message='Channel creation failed' />
          </Col>
        </Form>
      </Formik>
    </Col>
  );
}
