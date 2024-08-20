import {
    Box,
    Col, Label,
    ManagedToggleSwitchField as Checkbox,

    Text
} from '@tlon/indigo-react';
import { addTag, Association, Group, PermVariation, removeTag, metadataEdit, deSig, resourceFromPath } from '@urbit/api';
import { Form, Formik } from 'formik';
import _ from 'lodash';
import React from 'react';
import * as Yup from 'yup';
import { FormGroupChild } from '~/views/components/FormGroup';
import { shipSearchSchemaInGroup } from '~/views/components/ShipSearch';
import { ChannelWritePerms } from '../ChannelWritePerms';
import airlock from '~/logic/api';

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
      p={2}
      border={1}
      borderColor="lightBlue"
      borderRadius={1}
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
  const { group, association } = props;

  const writers = _.get(
    group?.tags,
    ['graph', association.resource, 'writers'],
    []
  );

  let [, , hostShip] = association.resource.split('/');
  hostShip = deSig(hostShip);

  const writePerms =
    writers.size === 0
      ? ('everyone' as const)
      : writers.size === 1 && writers.has(hostShip)
      ? ('self' as const)
      : ('subset' as const);

  const readerComments = association.metadata.vip === 'reader-comments';

  const initialValues = {
    writePerms,
    writers: [...writers]
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
    const allWriters = [...writers].map(w => `~${w}`);
    if (values.readerComments !== readerComments) {
      await airlock.poke(metadataEdit(association, {
        vip: values.readerComments ? 'reader-comments' : ''
      }));
    }

    if (values.writePerms === 'everyone') {
      if (writePerms === 'everyone') {
        actions.setStatus({ success: null });
        return;
      }
      await airlock.poke(removeTag(tag, resource, allWriters));
    } else if (values.writePerms === 'self') {
      if (writePerms === 'self') {
        actions.setStatus({ success: null });
        return;
      }
      const promises: Promise<any>[] = [];
      allWriters.length > 0 &&
        promises.push(airlock.poke(removeTag(tag, resource, allWriters)));
      promises.push(airlock.poke(addTag(resource, tag, [`~${hostShip}`])));
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
        promises.push(airlock.poke(removeTag(tag, resource, toRemove)));
      toAdd.length > 0 &&
        promises.push(airlock.poke(addTag(resource, tag, toAdd)));
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
        <FormGroupChild id="permissions" />
        <Col mx={4} mt={4} flexShrink={0} gapY={5}>
          <Col gapY={1} mt={0}>
            <Text id="permissions" fontWeight="bold" fontSize={2}>
              Permissions
            </Text>
            <Text gray>
              Add or remove read/write privileges to this channel. Group admins
              can always write to a channel
            </Text>
          </Col>
          <Col>
            <Label mb={2}>Permissions Summary</Label>
            <PermissionsSummary
              writersSize={writers.length}
              vip={association.metadata.vip}
            />
          </Col>
          <ChannelWritePerms />
          { ( association.metadata &&
              'graph' in association.metadata.config &&
              association.metadata.config.graph !== 'chat'
            )
            && (
            <Checkbox
              id="readerComments"
              label="Allow readers to comment"
              caption="If enabled, all members of the group can comment on this channel"
            />
          )}
        </Col>
      </Form>
    </Formik>
  );
}
