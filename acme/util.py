def all_subclasses(klass):
    subclasses = []

    for cls in klass.__subclasses__():
        subclasses.append(cls)

        if len(cls.__subclasses__()) > 0:
            subclasses.extend(all_subclasses(cls))

    return subclasses
