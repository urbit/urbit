import React from 'react';
import _ from 'lodash';
import * as Yup from 'yup';
import {
  Label,
  ManagedToggleSwitchField as Checkbox,
  Box,
  Col,
  Text
} from '@tlon/indigo-react';
import { Formik, Form } from 'formik';
import { PermVariation, Association, Group, Groups, Rolodex } from '@urbit/api';
import { shipSearchSchemaInGroup } from '~/views/components/ShipSearch';
import GlobalApi from '~/logic/api/global';
import { resourceFromPath } from '~/logic/lib/group';
import { FormSubmit } from '~/views/components/FormSubmit';
import { ChannelWritePerms } from '../ChannelWritePerms';

function PermissionsSummary(props: {
  writersSize: number;
  vip: PermVariation;
}) {
  const { writersSize, vip } = props;

  const description =
    writersSize === 0
      ? 'Currently, all members of the group can write to this channel'
      : `Currently, only ${writersSize} ship${
          writersSize > 1 ? 's' : ''
        } can write to this channel`;

  const vipDescription =
    vip === 'reader-comments' && writersSize !== 0
      ? '. All ships may comment'
      : '';

  return (
    <Box
      p="2"
      border="1"
      borderColor="lightBlue"
      borderRadius="1"
      backgroundColor="washedBlue"
    >
      <Text>
        {description}
        {vipDescription}
      </Text>
    </Box>
  );
}

interface GraphPermissionsProps {
  association: Association;
  group: Group;
  api: GlobalApi;
}

interface FormSchema {
  writePerms: 'self' | 'everyone' | 'subset';
  writers: string[];
  readerComments: boolean;
}

const formSchema = (members: string[]) => {
  return Yup.object({
    writePerms: Yup.string(),
    writers: shipSearchSchemaInGroup(members),
    readerComments: Yup.boolean()
  });
};

export function GraphPermissions(props: GraphPermissionsProps) {
  const { api, group, association } = props;

  const writers = _.get(
    group?.tags,
    ['graph', association.resource, 'writers'],
    new Set()
  );

  let [, , hostShip] = association.resource.split('/');
  hostShip = hostShip.slice(1);

  const writePerms =
    writers.size === 0
      ? ('everyone' as const)
      : writers.size === 1 && writers.has(hostShip)
      ? ('self' as const)
      : ('subset' as const);

  const readerComments = association.metadata.vip === 'reader-comments';

  const initialValues = {
    writePerms,
    writers: Array.from(writers)
      .filter(x => x !== hostShip),
    readerComments: association.metadata.vip === 'reader-comments'
  };

  const onSubmit = async (values: FormSchema, actions) => {
    values.writers = _.map(_.compact(values.writers), x => `~${x}`);
    const resource = resourceFromPath(association.group);
    const tag = {
      app: 'graph',
      resource: association.resource,
      tag: 'writers'
    };
    const allWriters = Array.from(writers).map(w => `~${w}`);
    if (values.readerComments !== readerComments) {
      await api.metadata.update(association, {
        vip: values.readerComments ? 'reader-comments' : ''
      });
    }

    if (values.writePerms === 'everyone') {
      if (writePerms === 'everyone') {
        actions.setStatus({ success: null });
        return;
      }
      await api.groups.removeTag(resource, tag, allWriters);
    } else if (values.writePerms === 'self') {
      if (writePerms === 'self') {
        actions.setStatus({ success: null });
        return;
      }
      const promises: Promise<any>[] = [];
      allWriters.length > 0 &&
        promises.push(api.groups.removeTag(resource, tag, allWriters));
      promises.push(api.groups.addTag(resource, tag, [`~${hostShip}`]));
      await Promise.all(promises);
      actions.setStatus({ success: null });
    } else if (values.writePerms === 'subset') {
      const toRemove = _.difference(allWriters, values.writers);

      const toAdd = [
        ..._.difference(values.writers, allWriters),
        `~${hostShip}`
      ];

      const promises: Promise<any>[] = [];
      toRemove.length > 0 &&
        promises.push(api.groups.removeTag(resource, tag, toRemove));
      toAdd.length > 0 &&
        promises.push(api.groups.addTag(resource, tag, toAdd));
      await Promise.all(promises);

      actions.setStatus({ success: null });
    }
  };

  const schema = formSchema(Array.from(group?.members ?? []));

  return (
    <Formik
      validationSchema={schema}
      initialValues={initialValues}
      onSubmit={onSubmit}
    >
      <Form style={{ display: 'contents' }}>
        <Col mt="4" flexShrink={0} gapY="5">
          <Col gapY="1" mt="0">
            <Text id="permissions" fontWeight="bold" fontSize="2">
              Permissions
            </Text>
            <Text gray>
              Add or remove read/write privileges to this channel. Group admins
              can always write to a channel
            </Text>
          </Col>
          <Col>
            <Label mb="2">Permissions Summary</Label>
            <PermissionsSummary
              writersSize={writers.size}
              vip={association.metadata.vip}
            />
          </Col>
          <ChannelWritePerms />
          {association.metadata.module !== 'chat' && (
            <Checkbox
              id="readerComments"
              label="Allow readers to comment"
              caption="If enabled, all members of the group can comment on this channel"
            />
          )}
          <FormSubmit>Update Permissions</FormSubmit>
        </Col>
      </Form>
    </Formik>
  );
}
