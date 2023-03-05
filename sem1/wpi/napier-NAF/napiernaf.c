#include "napiernaf.h"
#include <assert.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>

int max(int a, int b)
{
    if (a > b)
        return a;
    else
        return b;
}

int min(int a, int b)
{
    if (a < b)
        return a;
    else
        return b;
}

int abs(int n)
{
    if (n < 0)
        return -n;
    else
        return n;
}

int more(int n)
{
    if (n >= INT_MAX / 3 * 2)
        return n + 1;
    else
    {
        if (n == 1)
            n = 2;
        return n / 2 * 3 + 1;
    }
}

void iton(int x, int **a, int *n)
{
    int *result = NULL;
    int size = 0;
    int i = 0;
    for (int j = 0; x != 0; j++)
    {
        if (i == size)
        {
            size = more(size);
            result = realloc(result, (size_t)size * sizeof *result);
            assert(result != NULL);
        }
        switch (x % 4)
        {
        case 1:
        case -3:
            result[i] = j;
            i++;
            x = (x - 1) / 2;
            break;
        case 3:
        case -1:
            result[i] = -j - 1;
            i++;
            if (x < INT_MAX)
                x = (x + 1) / 2;
            else
            {
                x = x / 2;
                x++;
            }
            break;
        default:
            x /= 2;
        }
    }
    *a = result;
    *n = i;
}

int ntoi(int *a, int n)
{
    if (a == NULL || a[n - 1] > 31 || a[n - 1] < -32)
        return 0;
    if (a[n - 1] == 31)
    {
        if (n == 1 || a[n - 2] > -1)
            return 0;
    }
    else if (a[n - 1] == -32)
    {
        if (n > 1 && a[n - 2] < 0)
            return 0;
    }
    int x = 0;
    int k = 0;
    int two_to_power_k = -1;
    for (int i = 0; i < n; i++)
    {
        while (k < a[i] || k + 1 < -a[i])
        {
            k++;
            two_to_power_k *= 2;
        }
        if (a[i] >= 0)
            x -= two_to_power_k;
        else
            x += two_to_power_k;
    }
    return x;
}

int position(int n)
{
    if (n >= 0)
        return n;
    else
        return -n - 1;
}

void extend(int **c, int *cn)
{
    (*cn)++;
    *c = realloc(*c, (size_t)(*cn) * sizeof(**c));
}

void reduce(int **c, int *cn)
{
    (*cn)--;
    *c = realloc(*c, (size_t)(*cn) * sizeof(**c));
}

void add(int **c, int *cn, int n)
{
    if (!*cn || position(n) - position((*c)[*cn - 1]) > 1)
    {
        extend(c, cn);
        (*c)[*cn - 1] = n;
    }
    else
    {
        if (position(n) == position((*c)[*cn - 1]))
        {
            if ((*c)[*cn - 1] == n)
            {
                if (n >= 0)
                    (*c)[*cn - 1]++;
                else
                    (*c)[*cn - 1]--;
            }
            else
                reduce(c, cn);
        }
        else
        {
            if (n >= 0)
            {
                if ((*c)[*cn - 1] >= 0)
                {
                    (*c)[*cn - 1] = -(*c)[*cn - 1] - 1;
                    extend(c, cn);
                    (*c)[*cn - 1] = n + 1;
                }
                else
                    (*c)[*cn - 1] = -(*c)[*cn - 1] - 1;
            }
            else
            {
                if ((*c)[*cn - 1] >= 0)
                    (*c)[*cn - 1] = -(*c)[*cn - 1] - 1;
                else
                {
                    (*c)[*cn - 1] = -(*c)[*cn - 1] - 1;
                    extend(c, cn);
                    (*c)[*cn - 1] = n - 1;
                }
            }
        }
    }
}

void nadd(int *a, int an, int *b, int bn, int **c, int *cn)
{
    *cn = 0;
    *c = NULL;
    int i = 0, j = 0;
    while (i < an && j < bn)
    {
        if (position(a[i]) == position(b[j]))
        {
            if (a[i] == b[j])
            {
                if (a[i] >= 0)
                    add(c, cn, a[i] + 1);
                else
                    add(c, cn, a[i] - 1);
            }
            i++;
            j++;
        }
        else
        {
            if (position(a[i]) > position(b[j]))
            {
                add(c, cn, b[j]);
                j++;
            }
            else
            {
                add(c, cn, a[i]);
                i++;
            }
        }
    }
    while (i < an)
    {
        add(c, cn, a[i]);
        i++;
    }
    while (j < bn)
    {
        add(c, cn, b[j]);
        j++;
    }
}

void add_to_a(int **a, int *an, int *b, int bn)
{
    int *pom, pn;
    nadd(*a, *an, b, bn, &pom, &pn);
    free(*a);
    *a = pom;
    *an = pn;
}

void inverse(int **a, int an)
{
    for (int i = 0; i < an; i++)
        (*a)[i] = -(*a)[i] - 1;
}

void nsub(int *a, int an, int *b, int bn, int **c, int *cn)
{
    inverse(&b, bn);
    nadd(a, an, b, bn, c, cn);
    inverse(&b, bn);
}

void subtract_from_a(int **a, int *an, int *b, int bn)
{
    int *pom, pn;
    nsub(*a, *an, b, bn, &pom, &pn);
    free(*a);
    *a = pom;
    *an = pn;
}

void multiply_by_2_k_times(int *a, int an, int **pom, int *pn, int k)
{
    *pom = NULL;
    *pom = realloc(*pom, (size_t)(an) * sizeof(**pom));
    *pn = an;
    for (int i = 0; i < *pn; i++)
    {
        if (a[i] >= 0)
            (*pom)[i] = a[i] + k;
        else
            (*pom)[i] = a[i] - k;
    }
}

void multiply_a_by_2_k_times(int **a, int *an, int k)
{
    int *pom, pn; // tablica pomocnicza, w której przechowujemy a*2^k
    multiply_by_2_k_times(*a, *an, &pom, &pn, k);
    free(*a);
    *a = pom;
    *an = pn;
}

void nmul(int *a, int an, int *b, int bn, int **c, int *cn)
{
    int *pom, pn; // tablica, w której przechowujemy a*2^pozycja(b[i])
    *c = NULL;
    *cn = 0;
    for (int i = 0; i < bn; i++)
    {
        if (b[i] >= 0)
        {
            multiply_by_2_k_times(a, an, &pom, &pn, b[i]);
            add_to_a(c, cn, pom, pn);
        }
        else
        {
            multiply_by_2_k_times(a, an, &pom, &pn, -b[i] - 1);
            subtract_from_a(c, cn, pom, pn);
        }
        free(pom);
    }
}

void multiply_a(int **a, int *an, int *b, int bn)
{
    int *pom, pn;
    nmul(*a, *an, b, bn, &pom, &pn);
    free(*a);
    *a = pom;
    *an = pn;
}

int less(int *a, int an, int *b, int bn)
{
    if (an < bn)
        return !(less(b, bn, a, an));
    if (b == NULL)
        return (a != NULL && a[an - 1] < 0);
    int i = an - 1;
    while (i > an - bn && a[i] == b[i + bn - an])
        i--;
    if (a[i] != b[i + bn - an])
        return a[i] < b[i + bn - an];
    if (an > bn)
        return a[i - 1] < 0;
    return 0;
}

void divide_even_by_2(int **a, int an)
{
    for (int i = 0; i < an; i++)
    {
        if ((*a)[i] >= 0)
            (*a)[i]--;
        else
            (*a)[i]++;
    }
}

void ndivmodplus(int *a, int an, int *b, int bn, int **q, int *qn, int **r, int *rn)
{
    *q = NULL;
    *r = NULL;
    *rn = 0;
    *qn = 0;
    if (an > 0)
        *r = realloc(*r, (size_t)an * sizeof(**r));
    *rn = an;
    for (int i = 0; i < an; i++)
        (*r)[i] = a[i];
    if (!less(a, an, b, bn))
    {
        int cn = 0, *c = NULL;
        int k;
        while (!less(*r, *rn, b, bn))
        {
            k = position((*r)[*rn - 1]) - position(b[bn - 1]);
            multiply_by_2_k_times(b, bn, &c, &cn, k);
            if (less(*r, *rn, c, cn))
            {
                k--;
                divide_even_by_2(&c, cn);
            }
            subtract_from_a(r, rn, c, cn);
            add_to_a(q, qn, &k, 1);
            free(c);
        }
    }
}

void ndivmod(int *a, int an, int *b, int bn, int **q, int *qn, int **r, int *rn)
{
    int aNegative = (an == 0 || a[an - 1] < 0);
    int bNegative = b[bn - 1] < 0;
    if (aNegative)
        inverse(&a, an);
    if (bNegative)
        inverse(&b, bn);
    ndivmodplus(a, an, b, bn, q, qn, r, rn);
    if (aNegative && bNegative)
    {
        inverse(&a, an);
        if (*rn)
        {
            int one = 0;
            add_to_a(q, qn, &one, 1);
            int *pom, pn;
            nsub(b, bn, *r, *rn, &pom, &pn);
            free(*r);
            *r = pom;
            *rn = pn;
        }
        inverse(&b, bn);
    }
    else
    {
        if (aNegative)
        {
            inverse(&a, an);
            if (*rn)
            {
                int *tmp = NULL, pn = 0;
                iton(1, &tmp, &pn);
                add_to_a(q, qn, tmp, pn);
                free(tmp);
                nsub(b, bn, *r, *rn, &tmp, &pn);
                free(*r);
                *r = tmp;
                *rn = pn;
            }
            inverse(q, *qn);
        }
        if (bNegative)
        {
            inverse(&b, bn);
            inverse(q, *qn);
        }
    }
}

void divide_a(int **a, int *an, int *b, int bn)
{
    int *pom, pn;
    int *pom2, pn2;
    ndivmod(*a, *an, b, bn, &pom, &pn, &pom2, &pn2);
    free(*a);
    free(pom2);
    *a = pom;
    *an = pn;
}

void nexp(int *a, int an, int *b, int bn, int **c, int *cn)
{
    if (a)
    {
        iton(1, c, cn);
        int *divide, dn;
        iton(1, &divide, &dn);
        int *base = NULL;
        int pdn = an;
        base = malloc((size_t)pdn * sizeof(*base));
        for (int i = 0; i < pdn; i++)
            base[i] = a[i];
        int k = 0;
        for (int i = 0; i < bn; i++)
        {
            int pos = position(b[i]);
            while (pos > k)
            {
                k++;
                multiply_a(&base, &pdn, base, pdn);
            }
            if (b[i] >= 0)
            {
                multiply_a(c, cn, base, pdn);
                divide_a(c, cn, divide, dn);
                free(divide);
                iton(1, &divide, &dn);
            }
            else
            {
                multiply_a(&divide, &dn, base, pdn);
            }
        }
        free(base);
        free(divide);
    }
    else
    {
        *c = NULL;
        *cn = 0;
    }
}
