// https://manpages.ubuntu.com/manpages/impish/de/man2/mmap.2.html

#include <sys/mman.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#define handle_error(msg) do { perror(msg); exit(EXIT_FAILURE); } while (0)
#define ar1 "/home/tux/Schreibtisch/X11_Links.md"
#define ar2 3
#define ar3 10
#define ar4 15


int
main(int argc, char *argv[])
{
    char *addr;
    int fd;
    struct stat sb;
    off_t offset, pa_offset;
    size_t length=10;
    ssize_t s;

    fd = open(ar1, O_RDONLY);
    if (fd == -1)
        handle_error("open");

    if (fstat(fd, &sb) == -1)           /* Um die Dateigröße zu erhalten */
        handle_error("fstat");

    offset = ar2;
    pa_offset = offset & ~(sysconf(_SC_PAGE_SIZE) - 1);
    /* Versatz für mmap() muss an der Seite ausgerichtet sein */


    if (offset >= sb.st_size)
    {
        fprintf(stderr, "Versatz ist hinter dem Dateiende\n");
        exit(EXIT_FAILURE);
    }

    if (argc == 4)
    {
        length = atoi(argv[3]);
        if (offset + length > sb.st_size)
            length = sb.st_size - offset;
        /* Bytes hinter dem Dateiende können nicht angezeigt werden */

    }
    else        /* Kein Längen-Argument ==> Anzeige bis zum Dateiende */
    {
        length = sb.st_size - offset;
    }

    addr = mmap(NULL, length + offset - pa_offset, PROT_READ,
                MAP_PRIVATE, fd, pa_offset);
    if (addr == MAP_FAILED)
        handle_error("mmap");

    s = write(STDOUT_FILENO, addr + offset - pa_offset, length);
    if (s != length)
    {
        if (s == -1)
            handle_error("write");

        fprintf(stderr, "Schreiben unvollständig");
        exit(EXIT_FAILURE);
    }

    munmap(addr, length + offset - pa_offset);
    close(fd);

    exit(EXIT_SUCCESS);
}
