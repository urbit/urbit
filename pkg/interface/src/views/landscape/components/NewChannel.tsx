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
import { stringToSymbol, parentPath } from '~/logic/lib/util';
import { resourceFromPath } from '~/logic/lib/group';
import { Associations } from '~/types/metadata-update';
import { useWaitForProps } from '~/logic/lib/useWaitForProps';
import { Groups } from '~/types/group-update';
import { ShipSearch } from '~/views/components/ShipSearch';
import { Rolodex, Workspace } from '~/types';

interface FormSchema {
  name: string;
  description: string;
  ships: string[];
  moduleType: 'chat' | 'publish' | 'link';
  writers: string[];
}

const formSchema = (group, groups) => Yup.object({
  name: Yup.string().required('Channel must have a name'),
  description: Yup.string(),
  ships: Yup.array(Yup.string()),
  moduleType: Yup.string().required('Must choose channel type'),
  writers: Yup.array(Yup.string().test('ingroup', 'Writers must be in group',
    value => groups?.[group]?.members?.has(value)))
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
      const { name, description, moduleType, ships, writers } = values;
      switch (moduleType) {
        case 'chat':
        const appPath = `/~${window.ship}/${resId}`;
        const groupPath = group || `/ship${appPath}`;

        await api.chat.create(
          name,
          description,
          appPath,
          groupPath,
          { invite: { pending: ships.map(s => `~${s}`) } },
          ships.map(s => `~${s}`),
          true,
          false
          );
          break;
          case 'publish':
          if (writers.length > 0) {
            const resource = resourceFromPath(group);
            await api.groups.addTag(
              resource,
              { app: 'publish', tag: `writers-${resId}` },
              writers.map(s => `~${s}`)
              );
            }
            case 'link':
            if (group) {
              await api.graph.createManagedGraph(
                resId,
                name,
                description,
                group,
                moduleType
                );
              } else {
                await api.graph.createUnmanagedGraph(
                  resId,
                  name,
                  description,
                  { invite: { pending: ships.map(s => `~${s}`) } },
                  moduleType
                  );
                }
                break;
                default:
                console.log('fallthrough');
              }

              if (!group) {
                await waiter(p => Boolean(p?.groups?.[`/ship/~${window.ship}/${resId}`]));
              }
              if (moduleType === 'chat') {
                await waiter(p => Boolean(p?.chatSynced?.[`/~${window.ship}/${resId}`]));
              }
              actions.setStatus({ success: null });
              const resourceUrl = parentPath(location.pathname);
              history.push(
                `${resourceUrl}/resource/${moduleType}` +
                `${moduleType !== 'chat' ? '/ship' : ''}/~${window.ship}/${resId}`
                );
              } catch (e) {
                console.error(e);
                actions.setStatus({ error: 'Channel creation failed' });
              }
            };
            return (
              <Col overflowY="auto" p={3}>
                <Box pb='3' display={['block', 'none']} onClick={() => history.push(props.baseUrl)}>
                  {'<- Back'}
                </Box>
                <Box fontWeight="bold" mb={4} color="black">
                  New Channel
                </Box>
                <Formik
                  validationSchema={formSchema(group, groups)}
                  initialValues={{
                    moduleType: 'chat',
                    name: '',
                    description: '',
                    group: '',
                    ships: [],
                    writers: []
                  }}
                  onSubmit={onSubmit}
                >
                { ({ errors, values }) => <Form>
                  <Col
                  maxWidth="348px"
                  gapY="4"
                  >
                    <Col gapY="2">
                      <Box color="black" mb={2}>Channel Type</Box>
                      <Radio label="Chat" id="chat" name="moduleType" />
                      <Radio label="Notebook" id="publish" name="moduleType" />
                      <Radio label="Collection" id="link" name="moduleType" />
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
                    {(workspace?.type === 'home') &&
                    <ShipSearch
                    groups={props.groups}
                    contacts={props.contacts}
                    id="ships"
                    label="Invitees"
                    />}
                    {(workspace?.type !== 'home' && values.moduleType === 'publish') &&
                    <>
                    <ShipSearch
                    groups={props.groups}
                    contacts={props.contacts}
                    caption="Add writers to restrict who can write to this
                    notebook, or leave blank to allow all group members to write"
                    id="writers"
                    label="Writers"
                    />
                    {errors.writers &&
                    <>
                    <Row>
                      <Icon
                        color='white'
                        mr='2'
                        backgroundColor='red'
                        borderRadius='999px'
                        icon="ExclaimationMarkBold"
                      />
                      <Text color='red'>
                        {Array.from(new Set([...errors.writers]))}
                      </Text>
                      </Row>
                      </>
                      }
                    </>}
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
                </Form>}
                </Formik>
              </Col>
              );
            }
