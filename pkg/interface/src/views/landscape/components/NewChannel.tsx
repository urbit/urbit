import React from 'react';
import {
  Box,
  ManagedTextInputField as Input,
  Col,
  ManagedRadioButtonField as Radio,
  Text,
  Icon,
  Row
} from '@tlon/indigo-react';
import { Formik, Form } from 'formik';
import * as Yup from 'yup';
import GlobalApi from '~/logic/api/global';
import { AsyncButton } from '~/views/components/AsyncButton';
import { FormError } from '~/views/components/FormError';
import { RouteComponentProps } from 'react-router-dom';
import { stringToSymbol, parentPath, deSig } from '~/logic/lib/util';
import { resourceFromPath } from '~/logic/lib/group';
import { Associations } from '~/types/metadata-update';
import { useWaitForProps } from '~/logic/lib/useWaitForProps';
import { Groups } from '~/types/group-update';
import { ShipSearch, shipSearchSchemaInGroup, shipSearchSchema } from '~/views/components/ShipSearch';
import { Rolodex, Workspace } from '~/types';
import {IconRadio} from '~/views/components/IconRadio';
import {ChannelWriteFieldSchema, ChannelWritePerms} from './ChannelWritePerms';

type FormSchema = {
  name: string;
  description: string;
  ships: string[];
  moduleType: 'chat' | 'publish' | 'link';
} & ChannelWriteFieldSchema;

const formSchema = (members?: string[]) => Yup.object({
  name: Yup.string().required('Channel must have a name'),
  description: Yup.string(),
  ships: Yup.array(Yup.string()),
  moduleType: Yup.string().required('Must choose channel type'),
  writers: members ? shipSearchSchemaInGroup(members) : shipSearchSchema,
  writePerms: Yup.string()
});

interface NewChannelProps {
  api: GlobalApi;
  associations: Associations;
  contacts: Rolodex;
  chatSynced: any;
  groups: Groups;
  group?: string;
  workspace: Workspace;
}

export function NewChannel(props: NewChannelProps & RouteComponentProps) {
  const { history, api, group, workspace, groups } = props;

  const waiter = useWaitForProps(props, 5000);

  const onSubmit = async (values: FormSchema, actions) => {
    const resId: string = stringToSymbol(values.name)
    + ((workspace?.type !== 'home') ? `-${Math.floor(Math.random() * 10000)}`
    : '');
    try {
      let { name, description, moduleType, ships, writers } = values;
      if (group) {
        await api.graph.createManagedGraph(
          resId,
          name,
          description,
          group,
          moduleType
        );
        const tag = { 
          app: 'graph',
          resource: `/ship/~${window.ship}/${resId}`, 
          tag: 'writers' 
        };

        const resource = resourceFromPath(group); 
        writers = _.compact(writers);
        const us = `~${window.ship}`;
        if(values.writePerms === 'self') {
          await api.groups.addTag(resource, tag, [us]);
        } else if(values.writePerms === 'subset') {
          writers.push(us);
          await api.groups.addTag(resource, tag, writers);
        }

      } else {
        await api.graph.createUnmanagedGraph(
          resId,
          name,
          description,
          { invite: { pending: ships.map(s => `~${deSig(s)}`) } },
          moduleType
        );
      }

      if (!group) {
        await waiter(p => Boolean(p?.groups?.[`/ship/~${window.ship}/${resId}`]));
      }
      actions.setStatus({ success: null });
      const resourceUrl = parentPath(location.pathname);
      history.push(
        `${resourceUrl}/resource/${moduleType}/ship/~${window.ship}/${resId}`
      );
    } catch (e) {
      console.error(e);
      actions.setStatus({ error: 'Channel creation failed' });
    }
  };

  const members = group ? Array.from(groups[group]?.members).map(s => `~${s}`) : undefined;

  return (
    <Col overflowY="auto" p={3}>
      <Box pb='3' display={['block', 'none']} onClick={() => history.push(props.baseUrl)}>
        <Text fontSize='0' bold>{'<- Back'}</Text>
      </Box>
      <Box fontSize="1" fontWeight="bold" mb={4} color="black">
        New Channel
      </Box>
      <Formik
        validationSchema={formSchema(members)}
        initialValues={{
          moduleType: 'chat',
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
            <Col gapY="2">
              <Box fontSize="1" color="black" mb={2}>Channel Type</Box>
              <IconRadio icon="Chat" label="Chat" id="chat" name="moduleType" />
              <IconRadio icon="Publish" label="Notebook" id="publish" name="moduleType" />
              <IconRadio icon="Links" label="Collection" id="link" name="moduleType" />
            </Col>
            <Input
            id="name"
            label="Name"
            caption="Provide a name for your channel"
            placeholder="eg. My Channel"
            />
            <Input
            id="description"
            label="Description"
            caption="What's your channel about?"
            placeholder="Channel description"
            />
            {(workspace?.type === 'home') ? (
            <ShipSearch
            groups={props.groups}
            contacts={props.contacts}
            id="ships"
            label="Invitees"
          />) : (
            <ChannelWritePerms
              groups={props.groups}
              contacts={props.contacts}
            />
          )}
            
            <Box justifySelf="start">
              <AsyncButton
              primary
              loadingText="Creating..."
              type="submit"
              border
              >
                Create Channel
              </AsyncButton>
            </Box>
          <FormError message="Channel creation failed" />
          </Col>
        </Form>
      </Formik>
    </Col>
  );
}
