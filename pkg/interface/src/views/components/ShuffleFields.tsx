import React, { ReactNode, useMemo, useCallback } from "react";

import {
  FieldArray,
  FieldArrayRenderProps,
  Field,
  useFormikContext,
} from "formik";
import { Icon, Col, Row, Box } from "@tlon/indigo-react";

interface ShuffleFieldsProps<N extends string> {
  name: N;
  children: (index: number, props: FieldArrayRenderProps) => ReactNode;
}

type Value<I extends string, T> = {
  [k in I]: T[];
};

export function ShuffleFields<N extends string, T, F extends Value<N, T>>(
  props: ShuffleFieldsProps<N>
) {
  const { name, children } = props;
  const { values } = useFormikContext<F>();
  const fields: T[] = useMemo(() => values[name], [values, name]);

  return (
    <FieldArray
      name={name}
      render={(arrayHelpers) => {
        const goUp = (i: number) => () => {
          if(i > 0) {
            arrayHelpers.swap(i - 1, i);
          }
        };
        const goDown = (i: number) => () => {
          if(i < fields.length - 1) {
            arrayHelpers.swap(i + 1, i);

          }
        };
        return (
          <Box gridColumnGap="2" gridRowGap="3" display="grid" gridAutoRows="auto" gridTemplateColumns="32px 32px 1fr">
            {fields.map((field, i) => (
              <React.Fragment key={i}>
                <Icon width="3" height="3" icon="ChevronNorth" onClick={goUp(i)} />
                <Icon width="3" height="3" icon="ChevronSouth" onClick={goDown(i)} />
                {children(i, arrayHelpers)}
              </React.Fragment>
            ))}
          </Box>
        );
      }}
    />
  );
}
